
################## Item 1, Github June 27
###############################################
library(usethis)

# https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r/
#for now, just open project ...






################### end if item 1
##############################################

#install.packages("googledrive")
#install.packages('googlesheets4')
library("googledrive")
library(googlesheets4)
drive_auth()


#notes: I think I can save configuration of each analysis in a RDAta, with some note
#e.g. a dataframe of 'data' , 'path' ,'description' XXX


######## 2023 April, example of how to build up a google workflow from MHA analysis work

#how to check if they match? no copy error?
dataset_list <- c('lp_motivation','lp_rationale','lp_reflection','lp_reminder',
                  'mod_rationale','mod_interaction','mod_link')

original_name <- c('Long_Prompt_Motivation_Prototype_2',
                   'Long_Prompt_Rationale_Prototype_2',
                   'Long_Prompt_ReflectionCommit_Prototype_2',
                   'Long_prompt_Reminder_Prototype_2',
                   'MHAwave1ModularRationale',
                   'Modular_Interaction_MHA_Prototype',
                   'Modular_Link_MHA_Prototype')

link_list <- c('https://docs.google.com/spreadsheets/d/1DzLx0X9Iuax7z12Z6FeAQNSZxs5bUa1sMqBufpV00S4/edit?usp=share_link',
               'https://docs.google.com/spreadsheets/d/1EgVsv8ThoBf2_fgNPuiSxJtr__8NG6IleGET2lKZhfU/edit?usp=share_link',
               'https://docs.google.com/spreadsheets/d/1IOjksoLRa3VJay5VymppBwtjonFeQ1iB8MATANMz9u0/edit?usp=share_link',
               'https://docs.google.com/spreadsheets/d/1SPdje7w1eg91MVMP-3gwpGB9CI_MlWOUeZ6qu1AUtn8/edit?usp=share_link',
               'https://docs.google.com/spreadsheets/d/1IYMGLjttcbKR8rCgp6ab7z0kc1GUcdyy9o7tqTqcu9w/edit?usp=share_link',
               'https://docs.google.com/spreadsheets/d/1PnWbbdjV5_NX3xWL1w5WpPjBsVaQ0lilJTeQjIo1VM8/edit?usp=share_link',
               'https://docs.google.com/spreadsheets/d/1PtmemE3rv9WVFTfJztU7A-tlwEL102vWklc7BrcgNrQ/edit?usp=share_link')

df_names <- array(dim=c(length(link_list),2))
for(i in 1:length(link_list)){
  for(j in 1:2){
    df_name <- paste0('df_',dataset_list[i],'_',c('UR','TS')[j])
    df_names[i,j] <- df_name
    assign(df_name,read_sheet(link_list[i],sheet = j))
  }
}


df1 <- read_sheet(link_list[3])



path <- '~/Simulations/PostDiff/April6'


drive_upload('Rplot4.png',path=path,"1.png")
  
getwd()

df <- array(rbeta(100000*11,1,1),dim=c(100000,11))
df <- apply(df,1,max)
mean(rbeta(100000,1,2)>df)
