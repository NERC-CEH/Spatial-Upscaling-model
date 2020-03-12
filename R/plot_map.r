       
plot_map <- function(out_mat_pred,out_mat_var,east,north,resuse,model_variable,saveplots=FALSE,map_image_file,mod){

	   if(class(mod)!="try-error"){
	   
	   #windows(height=720,width=700)
        
          #split the plotting windoe into two - one for mean, one for variance
          par(mfrow=c(1,2))
          
          qtsm=round(quantile(c(out_mat_pred),c(0.1,0.9),na.rm=TRUE))
          brk=c(min(out_mat_pred,na.rm=TRUE),seq(qtsm[1],qtsm[2],length=9),max(out_mat_pred,na.rm=TRUE))
          #brk=c(seq(0,50,by=10),100)
          #cols=c("lightgoldenrod1","lightgoldenrod3","burlywood3","darkgoldenrod","lightsalmon4","darkorange4")
          cols=tim.colors(10)
          #produce image of the mean
          par(mai=c(0,0,0.4,0));image(east[[resuse]],rev(north[[resuse]]),(out_mat_pred[,1300:1]),asp=1,main="",col=cols,xaxt="n",breaks=brk,xlab="",yaxt="n",ylab="")
           
           ##add legend to the plot based on breaks
           lb=c()
           labs=as.character((round(brk,2))[-length(brk)])
           for(i in 1:length(labs)){
                 lb[i]=paste(as.character(labs[i])," - ",round(brk[i+1],2))
           }
           legend("topright",legend=as.character(lb),col=cols,pch=15,pt.cex=2.4,bty="n",title=model_variable,cex=0.75)


          #produce image of variance
          #find the quantiles of the variance in order to select appropriate breaks in legend 
          qts=round(quantile(c(out_mat_var),c(0.1,0.9),na.rm=TRUE))
          brk=c(seq(qts[1],qts[2],length=10),100)
          #specify colours as greyscale
          cols=rev(grey(seq(0.1,0.9,len=10)))
          
          #proudce image of variance
          par(mai=c(0,0,0.4,0));image(east[[resuse]],rev(north[[resuse]]),(out_mat_var[,1300:1]),asp=1,main="",col=cols,xaxt="n",breaks=brk,xlab="",yaxt="n",ylab="")
          
          #add legend to plot based on breaks and colours. 
          lb=c()
          labs=as.character(round(brk,2)[-length(brk)])
          for(i in 1:length(labs)){
                lb[i]=paste(as.character(labs[i])," - ",round(brk[i+1],2))
          }
          legend("topright",legend=as.character(lb),col=cols,pch=15,pt.cex=2.4,bty="n",title=paste("Variance of ",model_variable,sep=""),cex=0.75)
			}else{
		  #windows(height=720,width=700)
		   par(mfrow=c(1,2))
          par(mai=c(0,0,0.4,0))
          plot(c(0,1),c(0,1),type="n",main="Model Error",xaxt="n",yaxt="n",xlab="",ylab="")
          text(expand.grid(seq(0,1,len=5),seq(0,1,len=10)),"error",cex=0.7,col="grey")
          par(mai=c(0,0,0.4,0))
          plot(c(0,1),c(0,1),type="n",main="Model Error",xaxt="n",yaxt="n",xlab="",ylab="")
          text(expand.grid(seq(0,1,len=5),seq(0,1,len=10)),"error",cex=0.7,col="grey")
		  
		  }
		  
        #write out the graphics window
        if(saveplots){
			dev.copy(png,map_image_file,height=720,width=700)
			dev.off()
			}
		  
}		  

