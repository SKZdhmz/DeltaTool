## funkcije za delta tool
#############################################
#############################################
#############################################

delta_mjerenja<-function(godina,postaja=NULL,data_id=c("NO2","NOx","O3","PM10","PM2.5","SO2","NO"),fpath="F:\\Data/MJERENJA/",delta_pp="postaje_delta.xlsx",verzija="16") {
      # funkcija za vađenje podataka iz vesninih tablica i spremanje za delta tool format:
      # argumenti ###########
      # godina: trazena godina, 2011,2012,...
      # postaja: ime postaje, ime postaje preuzima se iz naziva vesninih file-ova. Funkcija ispisuje imena svih postaja
      # data_id: kem. spoj npr, "no2", "no" itd... ako ne nade ime stupca iz file-a, onda ispisuje sve spojeve u file i lako se provjeri ako stvarno postoji ili ne
      # fpath: put do mjerenja/vesninih xlsx-a, "svi podaci postaja od-do.xlsx"
      # delta_pp: popis postaj koji je potreban za program i delta tool
      # verzija: verzija vesninih tablica, na kraju file-a *16.xlsx, u buducnosti mozda 17,18,19....
      ###################
      
      #funckija za log. file i ispis na ekran
      log.file<-NULL
      log.fun<-function(string){
            cat(string,sep = " ; ",fill = T)
            log.file<<-paste0(log.file,paste(string,sep = "", collapse =" ; "),"\n")
      }
      
      # sve postaje u putu fpath
      data_all<-gsub(x=gsub(x = dir(path =fpath,all.files = T,recursive = T,pattern = paste0("svi.*",verzija,".xlsx")),pattern = "svi podaci ",replacement = ""),pattern = " [0-9][0-9].*",replacement = "")
      
      # print svih postaja
      log.fun("Popis svih mogucih postaja: pozadinske/urbane popis postaja")
      log.fun(data_all)
      
      # patetern za trazenje postaja, ako je postaja null uzima sve
      pattern<-paste("svi.*",postaja,".*",verzija,".xlsx",sep = "")
      
      #lista svih trazenih xlsx
      postaja<-dir(path = fpath,all.files = T,recursive = T,pattern =pattern)
      log.fun("popis odabranih postaja:")
      log.fun(gsub(x=gsub(x = postaja,pattern = "svi podaci ",replacement = ""),pattern = " [0-9][0-9].*",replacement = ""))
      # pridruzuje data_typ index-u sheeta (dh,d,m,g > 1,2,3,4), jer u sheetu 1 se nalaze satne, u 2 dnevne itd..
      typ<-1
      
      #startup_le.init file-a, config za delta tool
      startup_le<-paste0("[MODEL]\n;Year\n;frequency\n;Scale\n",godina,"\nhour\nurban\n[PARAMETERS]\n;Specie*type*measure unit\nNO2;GAS;ugm-3\nO3;GAS;ugm-3\nSO2;GAS;ugm-3\nNO;GAS;ugm-3\nPM10;PM;ugm-3\nPM25;PM;ugm-3\n[MONITORING]\nStation Code;Station Name;Station abbreviation;Altitude;Lon;Lat;GMTLag;Region;Station Type;Area Type;Siting;listOfvariables\n")
      
      #dir za model-scv
      dir.create("data/monitoring/le", showWarnings = FALSE,recursive = T)
      
      for(i in postaja){
            # provjera data_typ, pogledaj dali odgovara data_typ i sheet name
            if( getSheetNames(paste(fpath,i,sep=""))[typ]!="satne" ) error(paste("Trazene su satne, citaju se",getSheetNames(paste(fpath,i,sep=""))[typ]))
            
            #citanje excel
            data_xcl<-read.xlsx(xlsxFile =paste(fpath,i,sep=""),sheet = typ)
            
            # uzima se podskup, godina
            data_xcl<-data_xcl[data_xcl[,1]==godina,]
            
            # naziv stupca
            nazivi_stupca<-names(data_xcl)
            # mice se ekstenzija .?g/m3
            #nazivi_stupca<-gsub(x = nazivi_stupca,pattern = "..?g/m3",replacement = "")
            
            # u formatu delta toola nema tocke kod PM2.5...
            data_id_delta<-gsub(x = data_id,pattern = "\\.",replacement = "")
            # spajanje traženih pol. i mjerenih polutanata iz xlsx-a
            #spoj_ind<-match(tolower(data_id),tolower(nazivi_stupca))
            spoj_ind<-match(tolower(paste0(data_id,".µg/m3")),tolower(nazivi_stupca))
            # ako trazeni poll nema u xlsx-u
            if(sum(is.na(spoj_ind))>=1) {
                  
                  log.fun(paste("Trazenih polutanta: ",paste(data_id[is.na(spoj_ind)],collapse = "  ")," ,nema na postaji: ",gsub(x=gsub(x = i,pattern = "svi podaci ",replacement = ""),pattern = " [0-9][0-9].*",replacement = "")," popis svih polutanata u file-u:"))
                  log.fun(tolower(nazivi_stupca))
            }
            #indeks spojeva nadenih u file-u
            #spoj_ind<-spoj_ind[!is.na(spoj_ind)]
            
            # ako postoji barem jedan stupac u kojima su svi NA
            if (sum(colSums(is.na(data_xcl[,spoj_ind[!is.na(spoj_ind)]]))==nrow(data_xcl))>=1) {
                  
                  
                  spoj_nema<-spoj_ind[colSums(is.na(data_xcl[,spoj_ind[!is.na(spoj_ind)]]))==nrow(data_xcl)]
                  
                  log.fun(paste("Polutanti: ",paste(names(data_xcl)[spoj_nema],collapse = "  ")," ,na postaji: ",gsub(x=gsub(x = i,pattern = "svi podaci ",replacement = ""),pattern = " [0-9][0-9].*",replacement = "")," i godinu ", godina," su ~*NA*~ u svim terminima."))
                  
                  #spoj_ind<-spoj_ind[colSums(is.na(data_xcl[,spoj_ind[!is.na(spoj_ind)]]))<nrow(data_xcl)]
                  spoj_ind[colSums(is.na(data_xcl[,spoj_ind[!is.na(spoj_ind)]]))==nrow(data_xcl)]<-NA
            }
            
            
            # obrada svih trazenih POLL (ako postoje)
            if (length(spoj_ind[!is.na(spoj_ind)])>=1) { 
                  
                  
                  #podaci, subset trazenih poll
                  data_xcl<-data_xcl[,c(1:4,spoj_ind[!is.na(spoj_ind)])]
                  #imena stupaca
                  names(data_xcl)<-c("year","month","day","hour",data_id_delta[!is.na(spoj_ind)])
                  # delta tool nepostojece podatke zapisuje kao -999
                  data_xcl<-replace(data_xcl,is.na(data_xcl),-999)
                  ime_postaje<-gsub(x=gsub(x = i,pattern = "svi podaci ",replacement = ""),pattern = " [0-9][0-9].*",replacement = "")
                  
                  write.table(data_xcl,file = paste0("data/monitoring/le/",ime_postaje,".csv"), row.names = FALSE,sep=";",quote = F)
                  
                  # startup_le.ini
                  popispostaja<-read.xlsx(xlsxFile =delta_pp,colNames = T,sheet = 1)
                  popispostaja[popispostaja[,2]==ime_postaje,]
                  
                  # trazi postaju u file-u "postaje_delta.xlsx"
                  if(sum(popispostaja[,2]==ime_postaje)==1) {
                        temp<-popispostaja[popispostaja[,2]==ime_postaje,1:11]
                        temp["listOfvariables"]<-paste0(paste(data_id_delta[!is.na(spoj_ind)],collapse="*"),"*")
                        startup_le<-paste0(startup_le,paste(temp[1,],collapse=";"),";\n")
                  } else {
                        log.fun("Postaja,", ime_postaje," ne nalazi se ili se nalazi vise puta u datoteci 'delta_pp'=",delta_pp)
                  }
            }# kraj if ako nadje stupac s podacima
      } # kraj petlje po postajama
      # zapisuje startup_le.ini file, potreban za delta tool
      dir.create("resource", showWarnings = FALSE)
      write(startup_le,file = "resource/startup_le.ini")
      
      MyDeltaInput<-"startup_le.ini\nmodeling\\le\nmonitoring\\le\n\n# first line: startup\n# second line: modeling\n# third line: monitoring"
      write(MyDeltaInput,file = "resource/MyDeltaInput.dat")
      write(log.file,file = "log.file_mjerenja.txt")
}#kraj funkcije
#############################################
#############################################
#############################################

delta_gravimetrija<-function(godina,postaja=NULL,data_id=c("PM10","PM2.5"),fpath="F:\\Data/MJERENJA/IMI GRAVIMETRIJA 2003-2016.xlsx",delta_pp="postaje_delta.xlsx") {
      # funkcija za vađenje podataka iz "IMI GRAVIMETRIJA 2003-2016" i spremanje za delta tool format:
      # argumenti ###########
      # godina: trazena godina, 2011,2012,...
      # postaja: ime postaje, ime postaje preuzima se iz naziva vesninih file-ova. Funkcija ispisuje imena svih postaja
      # data_id: kem. spoj npr, "no2", "no" itd... ako ne nade ime stupca iz file-a, onda ispisuje sve spojeve u file i lako se provjeri ako stvarno postoji ili ne
      # fpath: put do gravimetrije "IMI GRAVIMETRIJA 2003-2016.xlsx"
      # delta_pp: popis postaj koji je potreban za program i delta tool
      # verzija: verzija vesninih tablica, na kraju file-a *16.xlsx, u buducnosti mozda 17,18,19....
      ###################
      
      #funckija za log. file i ispis na ekran
      log.file<-NULL
      log.fun<-function(string){
            cat(string,sep = " ; ",fill = T)
            log.file<<-paste0(log.file,paste(string,sep = "", collapse =" ; "),"\n")
      }
      
      # sve postaje u file-u IMI gravimetrija
      if (!file.exists(fpath)) stop(paste(fpath," -> file ne postoji definirajte drugi fpath:"))
      
      # patetern za trazenje postaja, ako je postaja null uzima sve
      pattern<-postaja
      
      # sve postaje u file-u
      postaja<-getSheetNames(fpath)
      # print svih postaja
      log.fun("Popis svih mogucih postaja (Gravimetrija):")
      log.fun(postaja)
      
      
      #lista svih trazenih xlsx
      postaja<-grep(paste(pattern,collapse="|"), postaja, value=TRUE,ignore.case = T)
      log.fun("popis odabranih postaja:")
      log.fun(postaja)
      # pridruzuje data_typ index-u sheeta (dh,d,m,g > 1,2,3,4), jer u sheetu 1 se nalaze satne, u 2 dnevne itd..
      
      #startup_le.init file-a, config za delta tool
      #vec je pripremljen kod 
      #startup_le<-paste0("[MODEL]\n;Year\n;frequency\n;Scale\n",godina,"\nhour\nurban\n[PARAMETERS]\n;Specie*type*measure unit\nNO2;GAS;ugm-3\nO3;GAS;ugm-3\nSO2;GAS;ugm-3\nNO;GAS;ugm-3\nPM10;PM;ugm-3\nPM25;PM;ugm-3\n[MONITORING]\nStation Code;Station Name;Station abbreviation;Altitude;Lon;Lat;GMTLag;Region;Station Type;Area Type;Siting;listOfvariables\n")
      startup_le<-NULL
      #dir za model-scv
      dir.create("data/monitoring/le", showWarnings = FALSE,recursive = T)
      
      for(i in postaja){
            #citanje excel
            data_xcl<-read.xlsx(xlsxFile =fpath,sheet = i)
            # miće Č ć ž š
            i<-gsub(x = i,pattern = "č",replacement = "c")
            i<-gsub(x = i,pattern = "š",replacement = "s")
            
            # uzima se podskup, godina
            data_xcl<-data_xcl[data_xcl[,1]==godina,]
            
            # naziv stupca
            nazivi_stupca<-names(data_xcl)

            # mice se ekstenzija .?g/m3
            #nazivi_stupca<-gsub(x = nazivi_stupca,pattern = "..?g/m3",replacement = "")
      #print(i)
      #print(names(data_xcl))
      #      }
            # u formatu delta toola nema tocke kod PM2.5...
            data_id_delta<-gsub(x = data_id,pattern = "\\.",replacement = "")
            # kod gravimetrije je PM25 zapisan kao PM2,5
            data_id_grav<-gsub(x = data_id,pattern = "\\.",replacement = ",")
            # spajanje traženih pol. i mjerenih polutanata iz xlsx-a
            spoj_ind<-match(tolower(paste0(data_id_grav,".s.korekcijom")),tolower(nazivi_stupca))
            spoj_ind_2<-match(tolower(data_id_grav),tolower(nazivi_stupca))
            #ako nema "PM s korekcijom" onda uzima "PM"
            spoj_ind[is.na(spoj_ind)]<-spoj_ind_2[is.na(spoj_ind)]
            # ako trazeni poll nema u xlsx-u
            if(sum(is.na(spoj_ind))>=1) {
                  
                  log.fun(paste("Trazenih polutanta: ",paste(data_id_grav[is.na(spoj_ind)],collapse = "  ")," ,nema na postaji: ",gsub(x=gsub(x = i,pattern = "svi podaci ",replacement = ""),pattern = " [0-9][0-9].*",replacement = "")," popis svih polutanata u file-u:"))
                  log.fun(tolower(nazivi_stupca))
            }
            #indeks spojeva nadenih u file-u
            #spoj_ind<-spoj_ind[!is.na(spoj_ind)]
            
            # ako postoji barem jedan stupac u kojima su svi NA
            if (!is.null(ncol(data_xcl[,spoj_ind[!is.na(spoj_ind)]]))) csum<-colSums(is.na(data_xcl[,spoj_ind[!is.na(spoj_ind)]]))
            if (is.null(ncol(data_xcl[,spoj_ind[!is.na(spoj_ind)]]))) csum<-sum(is.na(data_xcl[,spoj_ind[!is.na(spoj_ind)]]))
            if (sum(csum==nrow(data_xcl))>=1) {
      
                  spoj_nema<-spoj_ind[csum==nrow(data_xcl)]
                  
                  log.fun(paste("Polutanti: ",paste(names(data_xcl)[spoj_nema],collapse = "  ")," ,na postaji: ",gsub(x=gsub(x = i,pattern = "svi podaci ",replacement = ""),pattern = " [0-9][0-9].*",replacement = "")," i godinu ", godina," su ~*NA*~ u svim terminima."))
                  
                  #spoj_ind<-spoj_ind[colSums(is.na(data_xcl[,spoj_ind[!is.na(spoj_ind)]]))<nrow(data_xcl)]
                  spoj_ind[csum==nrow(data_xcl)]<-NA
            }
            
            
            # obrada svih trazenih POLL (ako postoje)
            if (length(spoj_ind[!is.na(spoj_ind)])>=1) { 
                  
                  
                  #podaci, subset trazenih poll
                  data_xcl<-data_xcl[,c(1:3,spoj_ind[!is.na(spoj_ind)])]
                  ####cbind(data_xcl[,c(1:3],rep(0:23,))
                  od<-ISOdate(data_xcl[1,1],data_xcl[1,2],data_xcl[1,3],hour =0)
                  do<-ISOdate(data_xcl[nrow(data_xcl),1],data_xcl[nrow(data_xcl),2],data_xcl[nrow(data_xcl),3],hour =23)
                  temp<-seq(od,do,by = "hour")
                  temp<-data.frame(as.POSIXlt(temp)$year+1900,as.POSIXlt(temp)$mon+1,as.POSIXlt(temp)$mday,as.POSIXlt(temp)$hour)
                  names(temp)<-c(names(data_xcl)[1:3],"sat")
                  a<-merge(temp,data_xcl,sort=F)
                  data_xcl<-merge(temp,data_xcl)

                  #imena stupaca
                  names(data_xcl)<-c("year","month","day","hour",data_id_delta[!is.na(spoj_ind)])
                  # delta tool nepostojece podatke zapisuje kao -999
                  data_xcl<-replace(data_xcl,is.na(data_xcl),-999)
                  ime_postaje<-paste0("GRV ",i)
                  
                  write.table(data_xcl,file = paste0("data/monitoring/le/",ime_postaje,".csv"), row.names = FALSE,sep=";",quote = F)
                  
                  # startup_le.ini
                  popispostaja<-read.xlsx(xlsxFile =delta_pp,colNames = T,sheet = 1)
                  popispostaja[popispostaja[,2]==ime_postaje,]
                  
                  # trazi postaju u file-u "postaje_delta.xlsx"
                  if(sum(popispostaja[,2]==ime_postaje)==1) {
                        temp<-popispostaja[popispostaja[,2]==ime_postaje,1:11]
                        temp["listOfvariables"]<-paste0(paste(data_id_delta[!is.na(spoj_ind)],collapse="*"),"*")
                        startup_le<-paste0(startup_le,paste(temp[1,],collapse=";"),";\n")
                  } else {
                        log.fun("Postaja,", ime_postaje," ne nalazi se ili se nalazi vise puta u datoteci 'delta_pp'=",delta_pp)
                  }
            }# kraj if ako nadje stupac s podacima
      } # kraj petlje po postajama
      # zapisuje startup_le.ini file, potreban za delta tool
      dir.create("resource", showWarnings = FALSE)
      write(startup_le,file = "resource/startup_le.ini",append = T)
      
      MyDeltaInput<-"startup_le.ini\nmodeling\\le\nmonitoring\\le\n\n# first line: startup\n# second line: modeling\n# third line: monitoring"
      write(MyDeltaInput,file = "resource/MyDeltaInput.dat")
      write(log.file,file = "log.file_gravimetrija.txt")
}#kraj funkcije
#############################################
#############################################
#############################################
delta_zoljan<-function(godina,data_id=c("PM10","PM2.5"),fpath="F:\\Data/MJERENJA/Zoljan-podaci.xls",delta_pp="postaje_delta.xlsx") {
      # funkcija za vađenje podataka za zoljan i spremanje za delta tool format:
      # argumenti ###########
      # godina: trazena godina, 2011,2012,...
      # data_id: kem. spoj npr, "no2", "no" itd... ako ne nade ime stupca iz file-a, onda ispisuje sve spojeve u file i lako se provjeri ako stvarno postoji ili ne
      # fpath: put do gravimetrije "Zoljan-podaci.xls"
      # delta_pp: popis postaj koji je potreban za program i delta tool
      ###################
      
      #funckija za log. file i ispis na ekran
      log.file<-NULL
      log.fun<-function(string){
            cat(string,sep = " ; ",fill = T)
            log.file<<-paste0(log.file,paste(string,sep = "", collapse =" ; "),"\n")
      }
      
      # sve postaje u file-u IMI gravimetrija
      if (!file.exists(fpath)) stop(paste(fpath," -> file ne postoji definirajte drugi fpath:"))
      
      # patetern za trazenje postaja, ako je postaja null uzima sve
      postaja<-"zoljan"
      pattern<-postaja
      
      # print svih postaja
      log.fun("Postaja:")
      log.fun(postaja)
      
      

      # pridruzuje data_typ index-u sheeta (dh,d,m,g > 1,2,3,4), jer u sheetu 1 se nalaze satne, u 2 dnevne itd..
      
      #startup_le.init file-a, config za delta tool
      #vec je pripremljen kod 
      #startup_le<-paste0("[MODEL]\n;Year\n;frequency\n;Scale\n",godina,"\nhour\nurban\n[PARAMETERS]\n;Specie*type*measure unit\nNO2;GAS;ugm-3\nO3;GAS;ugm-3\nSO2;GAS;ugm-3\nNO;GAS;ugm-3\nPM10;PM;ugm-3\nPM25;PM;ugm-3\n[MONITORING]\nStation Code;Station Name;Station abbreviation;Altitude;Lon;Lat;GMTLag;Region;Station Type;Area Type;Siting;listOfvariables\n")
      startup_le<-NULL
      #dir za model-scv
      dir.create("data/monitoring/le", showWarnings = FALSE,recursive = T)
      
      i<-postaja
      #citanje excel
      data_xcl<-read.xlsx(xlsxFile =fpath)
      read.table(file = fpath,header = T)
      # miće Č ć ž š
      i<-gsub(x = i,pattern = "č",replacement = "c")
      i<-gsub(x = i,pattern = "š",replacement = "s")
      
      # uzima se podskup, godina
      data_xcl<-data_xcl[data_xcl[,1]==godina,]
      
      # naziv stupca
      nazivi_stupca<-names(data_xcl)
      
      # mice se ekstenzija .?g/m3
      #nazivi_stupca<-gsub(x = nazivi_stupca,pattern = "..?g/m3",replacement = "")
      #print(i)
      #print(names(data_xcl))
      #      }
      # u formatu delta toola nema tocke kod PM2.5...
      data_id_delta<-gsub(x = data_id,pattern = "\\.",replacement = "")
      # kod gravimetrije je PM25 zapisan kao PM2,5
      data_id_grav<-gsub(x = data_id,pattern = "\\.",replacement = ",")
      # spajanje traženih pol. i mjerenih polutanata iz xlsx-a
      spoj_ind<-match(tolower(paste0(data_id_grav,".s.korekcijom")),tolower(nazivi_stupca))
      spoj_ind_2<-match(tolower(data_id_grav),tolower(nazivi_stupca))
      #ako nema "PM s korekcijom" onda uzima "PM"
      spoj_ind[is.na(spoj_ind)]<-spoj_ind_2[is.na(spoj_ind)]
      # ako trazeni poll nema u xlsx-u
      if(sum(is.na(spoj_ind))>=1) {
            
            log.fun(paste("Trazenih polutanta: ",paste(data_id_grav[is.na(spoj_ind)],collapse = "  ")," ,nema na postaji: ",gsub(x=gsub(x = i,pattern = "svi podaci ",replacement = ""),pattern = " [0-9][0-9].*",replacement = "")," popis svih polutanata u file-u:"))
            log.fun(tolower(nazivi_stupca))
      }
      #indeks spojeva nadenih u file-u
      #spoj_ind<-spoj_ind[!is.na(spoj_ind)]
      
      # ako postoji barem jedan stupac u kojima su svi NA
      if (!is.null(ncol(data_xcl[,spoj_ind[!is.na(spoj_ind)]]))) csum<-colSums(is.na(data_xcl[,spoj_ind[!is.na(spoj_ind)]]))
      if (is.null(ncol(data_xcl[,spoj_ind[!is.na(spoj_ind)]]))) csum<-sum(is.na(data_xcl[,spoj_ind[!is.na(spoj_ind)]]))
      if (sum(csum==nrow(data_xcl))>=1) {
            
            spoj_nema<-spoj_ind[csum==nrow(data_xcl)]
            
            log.fun(paste("Polutanti: ",paste(names(data_xcl)[spoj_nema],collapse = "  ")," ,na postaji: ",gsub(x=gsub(x = i,pattern = "svi podaci ",replacement = ""),pattern = " [0-9][0-9].*",replacement = "")," i godinu ", godina," su ~*NA*~ u svim terminima."))
            
            #spoj_ind<-spoj_ind[colSums(is.na(data_xcl[,spoj_ind[!is.na(spoj_ind)]]))<nrow(data_xcl)]
            spoj_ind[csum==nrow(data_xcl)]<-NA
      }
      
      
      # obrada svih trazenih POLL (ako postoje)
      if (length(spoj_ind[!is.na(spoj_ind)])>=1) { 
            
            
            #podaci, subset trazenih poll
            data_xcl<-data_xcl[,c(1:3,spoj_ind[!is.na(spoj_ind)])]
            ####cbind(data_xcl[,c(1:3],rep(0:23,))
            od<-ISOdate(data_xcl[1,1],data_xcl[1,2],data_xcl[1,3],hour =0)
            do<-ISOdate(data_xcl[nrow(data_xcl),1],data_xcl[nrow(data_xcl),2],data_xcl[nrow(data_xcl),3],hour =23)
            temp<-seq(od,do,by = "hour")
            temp<-data.frame(as.POSIXlt(temp)$year+1900,as.POSIXlt(temp)$mon+1,as.POSIXlt(temp)$mday,as.POSIXlt(temp)$hour)
            names(temp)<-c(names(data_xcl)[1:3],"sat")
            a<-merge(temp,data_xcl,sort=F)
            data_xcl<-merge(temp,data_xcl)
            
            #imena stupaca
            names(data_xcl)<-c("year","month","day","hour",data_id_delta[!is.na(spoj_ind)])
            # delta tool nepostojece podatke zapisuje kao -999
            data_xcl<-replace(data_xcl,is.na(data_xcl),-999)
            ime_postaje<-paste0("GRV ",i)
            
            write.table(data_xcl,file = paste0("data/monitoring/le/",ime_postaje,".csv"), row.names = FALSE,sep=";",quote = F)
            
            # startup_le.ini
            popispostaja<-read.xlsx(xlsxFile =delta_pp,colNames = T,sheet = 1)
            popispostaja[popispostaja[,2]==ime_postaje,]
            
            # trazi postaju u file-u "postaje_delta.xlsx"
            if(sum(popispostaja[,2]==ime_postaje)==1) {
                  temp<-popispostaja[popispostaja[,2]==ime_postaje,1:11]
                  temp["listOfvariables"]<-paste0(paste(data_id_delta[!is.na(spoj_ind)],collapse="*"),"*")
                  startup_le<-paste0(startup_le,paste(temp[1,],collapse=";"),";\n")
            } else {
                  log.fun("Postaja,", ime_postaje," ne nalazi se ili se nalazi vise puta u datoteci 'delta_pp'=",delta_pp)
            }
      }# kraj if ako nadje stupac s podacima
      # zapisuje startup_le.ini file, potreban za delta tool
      dir.create("resource", showWarnings = FALSE)
      write(startup_le,file = "resource/startup_le.ini",append = T)
      
      MyDeltaInput<-"startup_le.ini\nmodeling\\le\nmonitoring\\le\n\n# first line: startup\n# second line: modeling\n# third line: monitoring"
      write(MyDeltaInput,file = "resource/MyDeltaInput.dat")
      write(log.file,file = "log.file_gravimetrija.txt")
}#kraj funkcije

#############

#############



delta_model<-function(godina,data_id=c("NO2","NOx","O3","PM10","PM2.5","SO2","NO"),fpath="LE_conc-sfc-HR.nc",delta_pp="postaje_delta.xlsx") {
      # cita netCDF lotos-euros modela, trazi nn postaje i zapisuje csv
      # godina, odabrana godina
      # data_id, odabrani polutanti
      #fpath, put do izlaza modela lotos-euros
      #delta_pp, osnovni file za birranje postaja

      # cita postaje, "postaje_delta.xlsx"
      postaje<-read.xlsx(xlsxFile =delta_pp,sheet = 1)
      # load modela, "LE_conc-sfc-HR.nc"
      le<-nc_open(fpath)
      df<-data.frame()
      
      #podskup godina
      time<-as.POSIXlt(le$dim$time$vals[], origin = strsplit(le$dim$time$units,split = "since ")[[1]][2])
      ind_time<-which((time$year+1900)==godina)
      
      #trazi najblizeg susjeda, u modelu
      model_lon<-le$dim$longitude$vals[]
      model_lat<-le$dim$latitude$vals[]
      ind_lon<-sapply(postaje$Lon, function(x) {if (all(is.na(x))) {NA}  else {which.min(abs(x - model_lon))} }) 
      ind_lat<-sapply(postaje$Lat, function(x) {if (all(is.na(x))) {NA}  else {which.min(abs(x - model_lat))} })
      # prvi i zadnji dan u modelu za određenu godinu, cita iz nc-file-a
      prvi<-format(time[ind_time[1]], "%Y%m%d")
      zadnji<-format(time[rev(ind_time)[1]], "%Y%m%d")
      
      dir.create("data/modeling/le/csv/", showWarnings = FALSE,recursive = T)
      # petlja po postajama iz "postaje_delta.xlsx"
      for (i in 1:nrow(postaje)) {
            if (!is.na(ind_lon[i])) {
                  df<-data.frame(year=time[ind_time]$year+1900,month=time[ind_time]$mon+1,day=time[ind_time]$mday,hour=time[ind_time]$hour)
                  # 0-23 u 1-24
                  #ind_h<-df["hour"]==0
                  #temp23<-as.POSIXlt(time[ind_time][ind_h]-3600)
                  #df[ind_h,"year"]<-temp23$year+1900
                  #df[ind_h,"month"]<-temp23$mon+1
                  #df[ind_h,"day"]<-temp23$mday
                  #df[ind_h,"hour"]<-24
                  #
                  df["NO2"]<- ncvar_get(le,"no2")[ind_lon[i],ind_lat[i],ind_time]*(1/0.523)*1e9
                  df["O3"]<-ncvar_get(le,"o3")[ind_lon[i],ind_lat[i],ind_time]*(1/0.5)*1e9
                  df["SO2"]<-ncvar_get(le,"so2")[ind_lon[i],ind_lat[i],ind_time]*(1/0.355)*1e9
                  df["NO"]<-ncvar_get(le,"no")[ind_lon[i],ind_lat[i],ind_time]*(1/0.802)*1e9
                  df["PM10"]<-ncvar_get(le,"tpm10")[ind_lon[i],ind_lat[i],ind_time]*1e9
                  df["PM25"]<-ncvar_get(le,"tpm25")[ind_lon[i],ind_lat[i],ind_time]*1e9
                  #zapisuje csv file
                  write.table(df,file = paste0("data/modeling/le/csv/",postaje[i,2],".csv"), row.names = FALSE,sep=";",quote = F)
                  
                  
            }
      }
      
      # radi csv2cdf config, "InfoMODcsv2cdf_LE.txt"
      dir.create("conversion", showWarnings = FALSE,recursive = T)
      conv<-paste0("#SaveInfo File  => Do not remove the # lines <=\n#STARTUP Full Path to Startup.ini file\nC:\\Users\\Public\\Documents\\JRC_DELTA\\delta\\resource\\startup_le.ini\n#INITRUN\n",prvi,"\n#ENDRUN\n",zadnji,"\n#INPUT_DIR\nC:\\Users\\Public\\Documents\\JRC_DELTA\\delta\\data\\modeling\\le\\csv\\\n#INPUT_FILE_ID\n \n#OUTPUT_DIR\nC:\\Users\\Public\\Documents\\JRC_DELTA\\delta\\data\\modeling\\le\\\n#YEAR\n",godina,"\n#MODEL_NAME\nLE\n#POSTFIX\nTIME.cdf\n#OBSERVED_FLAG\n0\n#END")
      
      write(conv,file = "conversion/InfoMODcsv2cdf_LE.txt")
      
      #Dummy model --- delta tool se ne moze pokrenuti ako nema model u direktoriju, a da bismo dobili model potrebno je pokrenuti delta tool
      write.table(data.frame(), file=paste0('data/modeling/le/',godina,'_DUMMY_TIME.cdf'), col.names=FALSE)
      
}


