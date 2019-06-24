# LokDhaba

LokDhaba is a data dissemination and visualisation tool created by TCPD.

## Run LokDhaba from Docker Hub Container
```
docker run -d -p 3838:3838 --name lokdhaba -v /home/ubuntu/tcpd/:/srv/shiny-server/ -v /home/ubuntu/tcpd/:/var/log/ -it ashokatcpd/lokdhaba_shiny
```

## Run LokDhaba in Shiny
```
shiny::runApp('~/workspace/lokdhaba') (if ~/workspace/lokdhaba is where the lokdhaba git repo has been checked out)
```