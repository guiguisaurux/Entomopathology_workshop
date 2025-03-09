# configuration à refaire avec un nouvel ordinateur
git config --global user.name "Guillaume Saint-Jacques"
git config --global user.email "195578437+guiguisaurux@users.noreply.github.com"

# windows
git config --global core.autocrlf true

git config --global core.editor "nano -w"

git config --global init.defaultBranch main

#vérification
git --config --list 

#trucs cool de Git
#initialisation des contrôles de version
git init

#vérification du statut du projet
git status

if(length(grep("(?i)Git//bin", Sys.getenv("PATH"))) == 0) 
    Sys.setenv(PATH=paste0(Sys.getenv("PATH"),";C://Program Files//Git//bin")) 
    