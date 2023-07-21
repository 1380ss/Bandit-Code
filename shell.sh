git config --local -l
git config --local user.name '1380ss'
git config --local user.email 'to.li@mail.utoronto.ca'
git config --global -l
git config --list --local | grep user

git fetch origin master
git remote -v

git branch -r 

ls ./.git/refs/heads/ 
git fetch --all

# do following in future:
git fetch --all
git merge --allow-unrelated-histories
