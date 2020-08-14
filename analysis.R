#' ---
#' title: "Performance Metrics"
#' css: "production.css"
#' author: "Alex F. Bokov, Ph.D."
#' output:
#'   html_document:
#'     toc: false
#'     toc_float: true
#' ---
#' 
#+ init, echo=FALSE, message=FALSE, warning=FALSE,results='hide'
# init ----
.debug <- debug <- 0;
knitr::opts_chunk$set(echo=.debug>0,warning=.debug>0,message=.debug>0
                      ,results=if(.debug>0) 'markup' else 'hide');
.projpackages <- c('GGally','tableone','pander','dplyr','ggplot2');
.deps <- c( 'dictionary.R' ); 
.junk<-capture.output(source('./scripts/global.R',chdir=TRUE,echo=FALSE));
# Set some formatting options for this document
panderOptions('table.alignment.default','right');
panderOptions('table.alignment.rownames','right');
panderOptions('table.split.table',Inf);
panderOptions('p.wrap','');
panderOptions('p.copula',', and ');

.currentscript <- current_scriptname('analysis.R');
if(!exists('dat01')) dat01 <- get(names(inputdata)[1]);

#+ prepdata
# prepdata ----
# If there is no stop-date, it's somewhere in the future
dat01$nextdeadlineorfinald <- coalesce(dat01$nextdeadlineorfinald
                                       ,dat01$fundingdate
                                       ,Sys.Date()+100);
# If I don't know what my % effort is, assume it's 5%
dat01$bokov <- coalesce(dat01$bokov,0.05);

# Days between now and start of work on earliest documented grant
time<-min(dat01$`Started Preparation (bokov)`,na.rm=TRUE) %>% 
  seq(Sys.time(),by=60^2*24*7);
dat02 <- data.frame(time,source='Grant Pipeline',order=1
                    ,coverage = sapply(time,function(xx){
                      sum(subset(dat01,`What is due?`!='active' &
                                   `Started Preparation (bokov)`<=xx &
                                   nextdeadlineorfinald>=xx)$bokov,na.omit=T);
                      },simplify=T));
dat02 <- with(subset(dat01,`What is due?`%in% c('active','completed'))
              ,lapply(seq_along(rfafoanumberlink), function(ii){
                data.frame(time=dat02$time,source=rfafoanumberlink[ii]
                           ,order=ii+1
                           ,coverage=ifelse((is.na(fundingdate[ii]) |
                                               dat02$time>=fundingdate[ii]) &
                                              (is.na(enddate[ii]) |
                                                 dat02$time < enddate[ii])
                                            ,bokov[ii],0))
                })) %>% do.call(bind_rows,.) %>% 
  bind_rows(dat02,.) %>% arrange(order);

#+ plotdata
# plotdata ----
ggplot(dat02,aes(x=time,y=coverage)) + 
  geom_col(aes(fill=reorder(source,order),alpha=reorder(source,order))) + 
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_brewer(type='div',palette =3) + 
  scale_alpha_manual(values=c(0.2,rep(1,10))) + 
  labs(alpha='Funding Source',fill='Funding Source') + 
  theme_linedraw();

#+ repro
# repro ----
reproducibility <- tidbits:::git_status(print=F);
if(identical(reproducibility$status,'')){
  .repinfo0 <- '[%5$s commit](https://%3$s/%4$s/tree/%5$s) of the [%4$s](https://%3$s/%4$s) repository **%1$s** branch. You can download these scripts [here](https://%3$s/%4$s/archive/%5$s.zip)';
  .repinfo1 <- 'you will generate a report that is identical to this one';
} else {
  .repinfo0 <- '[%4$s](https://%3$s/%4$s) repository **%1$s** branch';
  .repinfo1 <- 'you should be able to generate a report that is similar to this one, but since the copy you are reading is a draft version, there may be differences due to subsequent revisions';
}
.repinfo0 <- with(reproducibility
                  ,sprintf(.repinfo0,branch,tracking,githost,repo,hash));
#' ## Reproducibility of these results.
#'
#' This report was automatically generated using scripts and lookup tables
#' publicly shared in the `r .repinfo0`. In addition you will need the following
#' data file:
#+ datafile, results='markdown'
inputdata[1] %>% cbind(file=basename(.),MD5sum=tools::md5sum(.)) %>%
  `[`(,-1) %>% pander;

#' The latest version of **`r basename(inputdata[1])`** can be obtained from 
#' [OneDrive](https://uthealthsa-my.sharepoint.com/:x:/r/personal/bokov_uthscsa_edu/Documents/00%20Metrics/Grants%20and%20Papers.xlsx?d=w54c27c0ff19a4fe494165f612f1269db&csf=1&web=1&e=SjVFxb)
#' if you were given access to that file.


#+ saveresults
# saveresults ----
save(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles));
#+ echo=FALSE, results='hide'
c()
