
###############
options(stringsAsFactors = FALSE)

#########read in data###################

args = commandArgs(trailingOnly=TRUE)


#example input#
#args[1]<-"--Protein"
#args[2]<-"50"
#args[3]<-"--Energy"
#args[4]<-"1300"
#args[5]<-"--Fat"
#args[6]<-"20"
#args[7]<-"--Super"
#args[8]<-"China_81204___China_72016___China_45215___China_51010___China_111101___USDASR_15085"
#args[9]<-"--Nutr"
#args[10]<-"Ca~4000___Fe~105___Zn~70___VD~10___VB12~0.01___VD~5___VE~60___Folate~1.5"
#args[11]<-"--foodfile"
#args[12]<-"child_food_select.txt"
#args[13]<-"--groupfile"
#args[14]<-"foodgroups.txt"
#args[15]<-"--output"
#args[16]<-"testoutput"

##test run
##Rscript food_recommend.R --Protein 50 --Energy 1300 --Fat 20 --Super China_81204___China_72016___China_45215___China_51010___China_111101___USDASR_15085 --Nutr Ca~4000___Fe~105___Zn~70___VD~10___VB12~0.01___VD~5___VE~60___Folate~1.5 --foodfile child_food_select.txt --groupfile foodgroups.txt --output testoutput --outputname testout

####

for(i in 1:length(args))
{if(args[i]=="--Protein")
 {Protein<-as.numeric(args[i+1])
  cat(paste0("Protein ",Protein,"\n"))
 }
 if(args[i]=="--Energy")
 {Energy<-as.numeric(args[i+1])
  cat(paste0("Energy ",Energy,"\n"))
 }
 if(args[i]=="--Fat")
 {Fat<-as.numeric(args[i+1])
  cat(paste0("Fat ",Fat,"\n"))
 }
 if(args[i]=="--Nutr")
 {req<-args[i+1]
  cat(paste0("Nutr ",req,"\n"))
  myreq<-unlist(strsplit(req,"___"))
  myreq_v<-vector()
  for(i in 1:length(myreq))
  {x<-unlist(strsplit(myreq[i],"~"))
   myreq_v[x[1]]<-as.numeric(x[2])
  }
  myreq_v_perday<-myreq_v/7
 }
 if(args[i]=="--Super")
 {superfoods<-args[i+1]
  cat(paste0("Super ",superfoods,"\n"))
  superfoods<-unlist(strsplit(superfoods,"___"))
 }
 if(args[i]=="--foodfile")
 {foodfile<-args[i+1]
  cat(paste0("foodfile ",foodfile,"\n"))
 }
 if(args[i]=="--groupfile")
 {groupfile<-args[i+1]
  cat(paste0("groupfile ",groupfile,"\n"))
 }
 if(args[i]=="--output")
 {output<-args[i+1]
  cat(paste0("output ",output,"\n")) 
 }
 if(args[i]=="--outputname")
 {outname<-args[i+1]
  cat(paste0("outname ",outname,"\n")) 
 }    
}


file1<-paste0(output,"/",outname,"_food_and_amount.txt")
file2<-paste0(output,"/",outname,"_food_and_amount.names.txt")
file3<-paste0(output,"/",outname,"_food_nutrition.txt")
              
#############STEP 1###########################################################
#############load data table##################################################

child_food<-read.table(foodfile,sep="\t",header=TRUE,
                       quote="\"",fill=TRUE,fileEncoding="UTF-8")
group_food<-read.table(groupfile,sep="\t",header=TRUE,
                       quote="\"",fill=TRUE,fileEncoding="UTF-8")
nutr_col<-which(colnames(child_food)=="Energy_kcal")
nutr_col2<-which(colnames(child_food)=="others")-1

###########STEP 2
###########define food groups by the gold standard food groups

group_name<-group_food[,2]

group_name<-strsplit(group_name,"___")

#get data for each group
group_info<-sub(".+category_lvl1_id: ","",child_food[,"others"])
group_info<-sub(";.+","",group_info)
group_info<-as.character(as.numeric(group_info))

group_lines<-list()
group_lines[[1]]<-which(group_info%in%group_name[[1]]) #milk
group_lines[[2]]<-which(group_info%in%group_name[[2]]) #soya
group_lines[[3]]<-which(group_info%in%group_name[[3]]) #meatfish
group_lines[[4]]<-which(group_info%in%group_name[[4]]) #egg
group_lines[[5]]<-which(group_info%in%group_name[[5]]) #veg
group_lines[[6]]<-which(group_info%in%group_name[[6]]) #frut
group_lines[[7]]<-which(group_info%in%group_name[[7]]) #main food
group_lines[[8]]<-which(group_info%in%group_name[[8]]) #peanuts
group_lines[[9]]<-which(!group_info%in%unlist(group_name)) #others


################

###########STEP 3
###########set the count of foods in each group per day (1 count = 100g)
ageadjust<-Energy/1800

###
milk<-2.5*ageadjust
soya<-0.5
meatfish<-1*ageadjust
egg<-0.5
veg<-3*ageadjust
frut<-2*ageadjust
main<-2*ageadjust
peanuts<-0.3
others<-0.8

everdayfoods<-c("China_101101","China_111101")  #milk egg everday

#####
eachgroup_count1<-c(1,1,2,1,3,1,2,1,2)  ###each day
eachgroup_count2<-c(1,1,2,1,3,1,2,1,2)  ###1 type milk, 2 types soya, 2 types fish
###1 type egg, 3 types vegatable, 1 type frute,
###2 types main food, 1 type peanut, 0-1 type other foods

##########STEP 4
##########initiate genetic algorithm functions

rand_days_food<-function(n=1,pre=NA,pre2=NA,needfood=0,Energy)
{food<-list()
 portion<-list()
 for(i in 1:n)
 {myfood<-rand_oneday_food(pre,pre2,needfood)
  myportion<-rand_portion(myfood)
  mynutr<-nutr(unlist(myfood),unlist(myportion))
  myEnergy<-sum(mynutr[,"Energy_kcal"])
  myadjust<-Energy/myEnergy
  myportion[["milk"]]<-myportion[["milk"]]*(2^(log2(myadjust)*0.8))
  myportion[["meatfish"]]<-myportion[["meatfish"]]*(2^(log2(myadjust)*0.5))
  myportion[["main"]]<-myportion[["main"]]*(2^(log2(myadjust)*0.8))
  myportion[["fruit"]]<-myportion[["fruit"]]*(2^(log2(myadjust)*0.8))
  for(j in 1:length(myportion))
  {myportion[[j]]<-round(myportion[[j]]/0.1)*0.1
  }
  food[[i]]<-myfood
  portion[[i]]<-myportion
 }
 myresult<-list()
 myresult[["food"]]<-food
 myresult[["portion"]]<-portion
 return(myresult)
}

rand_portion<-function(myfood)
{mynames<-c("milk","soya","meatfish","egg","veg","fruit","main","peanut","others")
 myresult<-list(milk/length(myfood[[1]])+sample(c(0,0.25),length(myfood[[1]]),replace=TRUE),
                soya/length(myfood[[2]])+sample(c(0,0.25),length(myfood[[2]]),replace=TRUE),
                meatfish/length(myfood[[3]])+sample(c(0,0.25),length(myfood[[3]]),replace=TRUE),
                egg/length(myfood[[4]])+sample(c(0,0.25),length(myfood[[4]]),replace=TRUE),
                veg/length(myfood[[5]])+sample(c(0,0.25),length(myfood[[5]]),replace=TRUE),
                frut/length(myfood[[6]])+sample(c(0,0.25),length(myfood[[6]]),replace=TRUE),
                main/length(myfood[[7]])+sample(c(0,0.25),length(myfood[[7]]),replace=TRUE),
                peanuts,
                others/length(myfood[[9]])+sample(c(0,0.25),length(myfood[[9]]),replace=TRUE)
             )
 names(myresult)<-mynames
 return(myresult)
}

rand_oneday_food<-function(pre=NA,pre2=NA,needfood=0)
{mynames<-c("milk","soya","meatfish","egg","veg","fruit","main","peanut","others")
 
 if(!is.na(pre[1]))
 {needfood<-c(needfood,sample(c(unlist(pre),unlist(pre2)),10,replace=TRUE))
 }
    
 x<-list()
 for(j in 1:9)
 {mycount<-sample(c(eachgroup_count1[j],eachgroup_count2[j]),1)
  x[[j]]<-c(intersect(needfood,group_lines[[j]]),
            group_lines[[j]][sample(length(group_lines[[j]]),mycount)])
  x[[j]]<-unique(x[[j]])[1:mycount]
 }
 names(x)<-mynames

 return(x)
}

nutr<-function(x,portions)
{   myresult<-list()
    for(i in 1:length(x))
    {myresult[[i]]<-portions[i]*(unlist(child_food[x[i],nutr_col:nutr_col2]))
    }
    do.call(rbind,myresult)
}

get_nut_highfood<-function(nut,topn=5)
{myresult<-vector()
 for(i in 1:length(nut))
 {myresult<-c(myresult,order(child_food[,nut[i]],na.last=TRUE,decreasing=TRUE)[1:topn])
 }
 return(sample(myresult,length(myresult)))
}



mysuperfood<-which(child_food[,"index"]%in%superfoods)
myeverdayfood<-which(child_food[,"index"]%in%everdayfoods)
flag=0
generation=0
while((flag == 0)&&(generation<100))
{
 if(generation==0)
 {n=7
  p1=NA;p2=NA
  highnutrfood<-vector()
 }else
 {n=7
  p1<-myfood[[myorder[1]]]
  p2<-myfood[[myorder[2]]]
  p1_portion<-myportion[[myorder[1]]]
  p2_portion<-myportion[[myorder[2]]]
  highnutrfood<-get_nut_highfood(nopassnutrs,topn=round(5/length(nopassnutrs)+1))
 }
 myfood<-list()
 myportion<-list()
 for(j in 1:7)
 {if(j<=length(mysuperfood))
  {myneedfood<-c(mysuperfood[j],myeverdayfood,sample(highnutrfood))
  }else
  {myneedfood<-c(myeverdayfood,sample(highnutrfood))
  }
  randresult=rand_days_food(n=1,pre=p1,pre2=p2,needfood=myneedfood,Energy=Energy)
  myfood[[j]]<-randresult$food[[1]]
  myportion[[j]]<-randresult$portion[[1]]
 }
 
 if(generation>0)
 {myfood[[myorder[1]]]<-p1
  myfood[[myorder[2]]]<-p1
  myportion[[myorder[1]]]<-p1_portion
  myportion[[myorder[2]]]<-p2_portion
 }
  
 mynutrs<-list()
 for(j in 1:7)
 {nutrs<-nutr(unlist(myfood[[j]]),unlist(myportion[[j]]))
  nutrs<-colSums(nutrs,na.rm=TRUE)
  mynutrs[[j]]<-nutrs
 }
 mynutrs<-do.call(rbind,mynutrs)
 
 totalreqnutrs<-(colSums(mynutrs[,names(myreq_v)],na.rm=TRUE))
 passnutrs<-which(totalreqnutrs-myreq_v>0)
 nopassnutrs<-names(myreq_v)[!names(myreq_v)%in%names(passnutrs)]
 
 if(length(passnutrs)>=length(myreq_v))
 {flag=1
 }else
 {counts<-lapply(1:7,function(j){
  length(which((mynutrs[j,names(myreq_v)]-myreq_v_perday)>0))
  })
  counts<-unlist(counts)
  myorder<-rev(order(counts))
 }
 
 generation<-generation+1
}

cat("#Everday food and amount",file=file1,sep="\n")
cat("#Everday food and amount",file=file2,sep="\n")
for(i in 1:length(myfood))
{foods<-child_food[unlist(myfood[[i]]),"index"]
 food_names<-child_food[unlist(myfood[[i]]),"key_name_cn"]
 amounts<-unlist(myportion[[i]])*100
 
 cat(paste0(i,"\t"),file=file1,sep="\t",append = TRUE)
 cat(as.vector(rbind(foods,amounts)),file=file1,sep="\t",append = TRUE)
 cat("\n",file=file1,append = TRUE)
 
 cat(paste0(i,"\t"),file=file2,sep="\t",append = TRUE)
 cat(as.vector(rbind(food_names,paste0(amounts,"g"))),file=file2,sep="\t",append = TRUE)
 cat("\n",file=file2,append = TRUE)
}


cat("#Food nutrition",file=file3,sep="\n")
cat(c("Day",colnames(mynutrs)),file=file3,sep="\t",append = TRUE)
cat("\n",file=file3,append = TRUE)
for(i in 1:7)
{
 cat(paste0(i,"\t"),file=file3,sep="\t",append = TRUE)
 cat(mynutrs[i,],file=file3,sep="\t",append = TRUE)
 cat("\n",file=file3,append = TRUE)
}
cat("Total\t",file=file3,sep="\t",append = TRUE)
cat(colSums(mynutrs),file=file3,sep="\t",append = TRUE)
cat("\n",file=file3,append = TRUE)



