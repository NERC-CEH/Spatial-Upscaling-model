
plot_fit <- function(mod,fit_image_file,saveplots=FALSE){
		 if(class(mod)!="try-error"){
        ## produce a goodness of fit plots
        #windows(height=320,width=700)
          par(mfrow=c(1,2))
          par(mai=c(1.02, 0.82 ,0.82, 0.42))
          #scatter plot of observed versus fitted values
          plot(mod$gam$y,mod$gam$fitted.values,xlab="Observed Values",ylab="Fitted Values")
          abline(a=0,b=1,col="red",lwd=2)
          #historgram of model residuals
          hist(mod$gam$y-mod$gam$fitted.values,main="",xlab="Model Residuals")
		}else{
			#windows(height=320,width=700)
			par(mfrow=c(1,2))
			par(mai=c(1.02, 0.82, 0.82, 0.42))
          plot(0,type="n",xlab="",ylab="",main="Model Error")
          plot(0,type="n",xlab="",ylab="",main="Model Error")
		}
        if(saveplots){
			dev.copy(png,fit_image_file,height=320,width=700)
			dev.off()
			}

}
        
########