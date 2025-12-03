docker build . -t clustering --platform=linux/amd64

docker run --platform linux/amd64 \
  -e USERID=$(id -u) \
  -e GROUPID=$(id -g) \
  -v $(pwd):/home/rstudio/work \
  -v $HOME/.ssh:/home/rstudio/.ssh \
  -v $HOME/.gitconfig:/home/rstudio/.gitconfig \
  -w /home/rstudio/work \
  -p 8787:8787 -it clustering