# Case Outcome Simulator (COS)
This repository contains the code needed for the COS. The COS is a web application (built on R and Shiny) intended to allow targeted user groups to interact with a model of child welfare case outcomes for Washington state. Specifically, user groups interact with the application by adjusting model inputs and then observing as the application simulates likelihoods for the four key case outcomes (Reunification, Adoption, Guardianship, and Emancipation) based on those inputs.

There are different application "modes" for different user groups, all built upon the same application infrastructure but with user interface features and visualizations tailored to the user groups.

As of this writing, the available modes are:
* Explore Mode: designed for academics and policy makers, appropriate for observing how outcome likelihoods change across a range of values
* Single Case Mode: designed for case workers, appropriate for simulating the likelihood of outcomes based on inputs for a single case

It is worth noting that the COS is a specific instance of an application built to be usable with a most multinomial models/datasets. Changing the model/dataset simply requires updating the relevant sections of the application config file [IN PROGRESS] to point the application at the correct data object, specify the desired formula, and provide key coefficient information (impacts what variables users can see and interact with).

## Setting Up the AWS EC2 Instance
Currently this application is deployed via [Amazon's EC2 service](http://aws.amazon.com/ec2/?sc_channel=PS&sc_campaign=acquisition_US&sc_publisher=google&sc_medium=ec2_b&sc_content=ec2_e&sc_detail=amazon.ec2&sc_category=ec2&sc_segment=53611778562&sc_matchtype=e&sc_country=US&s_kwcid=AL!4422!3!53611778562!e!!g!!amazon.ec2&ef_id=VTlq7QAAAQOLjYDQ:20150511210335:s). What follows will describe how to setup a (free) micro instance for the COS using the EC2 Ubuntu AMI.

### Launching the Instance
http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-instance_linux.html
- we've been using the Ubuntu AMI

### Authorize Inbound Traffic to the Instance
http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/authorizing-access-to-an-instance.html
- actually probably okay by default; instead should double check that instance is secure enough

### Setup Putty to Access the Instance
http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/putty.html	
- note especially that you need to convert the private key file

### Setup a Swapfile
We need to this especially on the micro instances, which may easily run out of RAM during certain R operations 

(e.g., installing larger R packages).
http://serverfault.com/questions/218750/why-dont-ec2-ubuntu-images-have-swap

### Setup the R Mirror
This insures we get the most up to date version of R and packages.
```
sudo vi /etc/apt/sources.list.d/sources.list
```
Add this line.
```
deb http://cran.cs.wwu.edu/bin/linux/ubuntu trusty/
```
http://cran.r-project.org/mirrors.html

### Install and Configure R, Shiny, and Shiny Server
https://github.com/chrisrzhou/RShiny-EC2Bootstrap#install-r
```
sudo apt-get update
sudo apt-get install r-base
sudo apt-get install r-base-dev
sudo apt-get install 
```
- we'll want to give write permissions for the Ubuntu user account to the default R library pathway so that 

installing/managing libraries is simplified
```
sudo chmod 777 /usr/local/lib/R/site-library
```
- do a quick update request (```sudo apt-get update```) before installing the packages to make sure the R version 

is up to date
- for the R packages, just install "shiny" and "packrat" - "packrat" will handle installing the rest once we clone 

our project (though we'll need to do some fiddling to get the important "cairo" package to run)
- make a folder in the Ubuntu directory that will contain the desired Shiny apps (e.g., mkdir 

/home/ubuntu/shiny_apps) and that will contain Shiny logs (e.g., mkdir /home/ubuntu/shiny_logs)
- adjust the shiny config file to (a) point to the appropriate directories and (b) run as 'ubuntu' (sudo vi 

/etc/shiny-server/shiny-server.conf)
- useful resource: http://rstudio.github.io/shiny-server/latest/#default-configuration
- open the shiny-server port '3838' (http://stackoverflow.com/questions/5004159/opening-port-80-ec2-amazon-web-

services/10454688#10454688)

NOTE: You might get authentication warnings. Ignore these (say yes where needed or ignore them where they're just 

messages) unless you know how to properly setup the authentication process for R repository we're drawing our files 

from.


### Install and Configure Git
```sudo apt-get install git```
Then we need to configure git to 

### Cloning and Configuring the Shiny App(s)
At this point, we have R, Shiny, and Shiny Server installed and configured to play nicely with the default ubuntu 

account. 

Now we need to install our app(s), any packages they need, and any resources those packages need.

The easiest way to use this to clone the project repository into your designated shiny_apps folder. This also makes 

it incredibly easy to update the app with pull requests.

NOTE: You may need to take steps to allow pull/push from your git repository. This may require making/getting keys 

from your GitHub account and assigning these keys properly on the EC2 server. I had issues on my first install but 

not my second, so I unfortunately cannot describe how to solve this issue.

### The Case Outcome Simulator (COS)
COS uses Packrat to manage its package dependencies. All we need to do is clone the project and then start R in the 

COS application directory ("./ubuntu/sim-tool/COS Shiny project") and Packrat will install all of the relevant 

packages from their binaries (which Packrat keeps copies of). 

However, the Cairo package is special and attempts to install it will fail unless the correct resources are added 

to our EC2 server.

I THINK the only packages missing from the default EC2 setup are these:
```
sudo apt-get install libcairo2-dev
sudo apt-get install libcairo2-dev
```

After you install these, just initialize R in the COS Shiny project folder. Packrat should automatically try to 

install all the packages. If it doesn't, first make sure the working directory is correct (e.g., 

/home/ubuntu/shiny_apps/sim-tool/COS shiny project) and then manually force the update (packrat::restore()).

If any errors, you'll need to problem shoot or install those packages manually (e.g., install.packages()).

### Getting the link to your app...
[Public DNS]:[Port Number]
ec2-52-8-38-141.us-west-1.compute.amazonaws.com:3838

If the app of interest is in a sub-directory (most likely), then you will want to visit this link and click through 

the auto-generated index to get the full link to your app.

### TROUBLESHOOTING
If you get EADDRINUSE errors... try changing the port Shiny is using (e.g., to 3939). Don't forget that you need to 

add this port to the EC2 inbound security group AND that this changes the link people need to use to reach the app.


