############################################################################
# computer information:
# 台式机, 9代i7, GTX2060
# wire needed: 15M
#
# clean environment, smoke forbidden
# vending machine needed (not considered)
############################################################################
CashFlow <- function(usertime=2.629, #(h/week) 1 ~ 4
		     userprice=11.01, #(yuan/h) 5 ~ 15
		     privateprice=27.08, #(yuan/h) 15 ~ 35
		     cpsalary=3360, # 3000 ~ 4500
		     adminsalary=3000, # 1500 ~ 5000
		     floatrate=0.15, # 0.05 ~ 0.3
		     Decoration=25283.84, # 23303.84 ~ 27263.84
		     rpd=21.9, # 18.5 ~ 31.1
		     Write=F # write into csv
		     ){

	# number of students (?)
	# 274.19+516.91*log(year - 2013)
	# n1 = sum(n(2016:2019))

	#  313  600
	#  806  951 1146 1230 real data
	# 1280 1349 1410 1464 estimated
	nstudent = c(806, 951, 1146, 1230 ,1280, 1349, 1410, 1464)
	n <- c(sum(nstudent[1:4]),
	       sum(nstudent[2:5]),
	       sum(nstudent[3:6]),
	       sum(nstudent[4:7]),
	       sum(nstudent[5:8]))

	# from questionnaire
	usersrate <- 0.3495
	privaterate <- 0.2427
	ppu <- 0.6944

	maxusertime <- 4 #(h/week)

	privateseat <- 5 #(/room)

	privatetime <- usertime / 5

	# Life time of the project(estimated)
	ProjectHP <- 5 #(year)

	############################################################################
	# Revenue related(survey)
	vc <- 0 # variable cost per hour (assuming no variable cost)
	# Gross Profit per week
	Profit <- (userprice - vc) * usertime * usersrate * n +
		(privateprice - privateseat * vc) * privatetime * privaterate * n
	# transfered into per year
	Profit <- Profit * 365 / 7

	############################################################################
	# Equipment related(search and estimated)
	ComputerNum <<- floor(usersrate * maxusertime * n[1] / 7 / 12)
	PrivateRoom <<- ceiling(usersrate * maxusertime * n[1] / 7 / 12 *
			       ppu / (1+ppu) / 5)
	size <<- 20 * PrivateRoom + 2 * (ComputerNum - 5 * PrivateRoom)
	# Computer, Screen, desk&chair, mouse&keyboard&headset
	PricePerComputer <- 5999 + 1400 + 700 + 200
	AC <- 1169*6+4199
	Instuall <- 60+80+100+200
	digital <- PricePerComputer * ComputerNum
	BeginCost <- digital+AC+Instuall
	CapitalHP <- 5 #(year) fixed
	salvage <- (5999*0.4 + 500 + 200) * ComputerNum + 4199*0.2
	deprecation <- c(rep(digital/3,3),0,0) + rep(AC/5,5)

	############################################################################
	# Fixed Cost(search and estimated) (?)
	rent <- size*rpd*365

	salary <- cpsalary*4*12 +
		adminsalary*5*12

	netfee <- 269*12

	utility <- 0.98*3.668*ComputerNum*2*30*12

	item <- 0.9*ComputerNum+9+15*(6+4)

	FC <- rent + salary + utility + item

	# DeprecationSquence
	# DS <- c(rep(deprecation, CapitalHP),rep(0, ProjectHP-CapitalHP))

	# EBIT
	EBIT <- Profit - rep(FC, ProjectHP) - deprecation

	############################################################################
	# tax rate(fixed)
	t <- 0.25
	# 0.02*(EBIT>5000*12)+0.03*(EBIT>30000*12)

	# unlevered net income
	NI <- (1-t)*EBIT

	# Free cash flow
	NI <- NI + deprecation

	# Net Working Capital
	NWC <- 0 # (not considered)

	# Required return
	r = 0.0475 * (1+floatrate)

	FCF <- sum(NI*(1+r)^(-1:-ProjectHP))-
		NWC - BeginCost +
		(NWC+salvage)*(1+r)^(-ProjectHP-1)

	if (Write){
		m<<-matrix(c(1:5,Profit,rep(FC,5),deprecation,EBIT,NI),ncol=5,byrow=T)
		rownames(m)<<-c("Year","Gross Profit","Fixed Cost","Deprecation",
				"EBIT","Unlevered Net Income")
		m<<-cbind(rep(NA,6),m)
		m<<-data.frame(m)
		m[1,1]<<-0
		m["Equipment and Beginning Cost",1] <<- BeginCost
		m["Salvage",6] <<- salvage
		m["FCF",1] <<- FCF

		write.csv(m,"FINproj.csv")
	}
	cat(FCF,"\n")
	return(FCF)
}
############################################################################
# sensitivity analysis
# usertime userprice privateprice
# salary
senalys <- function(){
	Prediction <- CashFlow()
	range <- 8*10^6
	# less
	Less <- c(
		  CashFlow(usertime=1) - Prediction,
		  CashFlow(userprice=5) - Prediction,
		  CashFlow(rpd=31.1) - Prediction,
		  CashFlow(privateprice=15) - Prediction,
		  CashFlow(cpsalary=4500, adminsalary=5000) - Prediction,
		  CashFlow(floatrate=0.3) - Prediction,
		  CashFlow(Decoration=27263.84) - Prediction
		  )

	# par(pin=c(8,5))

	barplot(Less, axes = FALSE, horiz = T, col = 'red', border ='white',
		xlim = c(-range,range) )
	#,ylim = c(0.5,9.2)

	# more
	More <- c(
		  CashFlow(usertime=4) - Prediction,
		  CashFlow(userprice=15) - Prediction,
		  CashFlow(rpd=18.5) - Prediction,
		  CashFlow(privateprice=35) - Prediction,
		  CashFlow(cpsalary=3000, adminsalary=1500) - Prediction,
		  CashFlow(floatrate=0.05) - Prediction,
		  CashFlow(Decoration=23303.84) - Prediction
		  )

	barplot(More, axes = FALSE, horiz = T, col = 'green', border ='white',
		add = TRUE)

	abline(v = -Prediction ,lwd=2,col='black')
	abline(v = -0 ,lwd=2,col='white')
	# axis(3, lwd = 2, tick = T, at = seq(-range,range,10^6),col='gray',
	# labels = c(round(seq(Prediction-range,Prediction+range,10^6), 1)), las = 1)
	axis(1, lwd = 2, tick = T, at = seq(-range,range,10^6),col = 'gray',
	     labels = c(round(seq(Prediction-range,Prediction+range,10^6), 1)), las = 1)


	legend("topright",
	       legend=c('NPV under the bestcase assumption for each parameter.',
			'NPV under the worstcase assumption for each parameter.'),
	       col=c('red','green'),pch=15,bty='o',xpd=TRUE)
	# , horiz = T,box.col='gray')
}

senalys()
CashFlow(Write=T)
