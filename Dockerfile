# Dockerized version of DECONV > DE transformation code, provided by Harvard.
FROM r-base

COPY ./foodgroups.txt /foodgroups.txt
COPY ./child_food_select.txt /child_food_select.txt
COPY ./food_recommend.R /food_recommend.R

ENTRYPOINT ["Rscript","/food_recommend.R"]
