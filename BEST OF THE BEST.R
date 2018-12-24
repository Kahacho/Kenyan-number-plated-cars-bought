cars<-function(numberplate_1,numberplate_2){

x1<-substitute(numberplate_1)
n_1<-unlist(strsplit(toString(x1),""))

x2<-substitute(numberplate_2)
n_2<-unlist(strsplit(toString(x2),""))

#Assigning each letter a number excluding I and O
a<-data.frame(c(LETTERS[c(-9,-15)]))

#The second elements of a Kenyan number plate
b<-which(a[1:nrow(a),]==n_1[2])
c<-which(a[1:nrow(a),]==n_2[2])

#The third elements of a Kenyan number plate
d<-which(a[1:nrow(a),]==n_1[3])
e<-which(a[1:nrow(a),]==n_2[3])

#The seventh elements of a Kenyan number plate
f<-which(a[1:nrow(a),]==n_1[7])
g<-which(a[1:nrow(a),]==n_2[7])

#The numeric elements in a Kenyan number plate
n1<-as.numeric(paste(n_1[4:6],collapse=""))
n2<-as.numeric(paste(n_2[4:6],collapse=""))

#Number plates location
n1_location<-999*(f-1)+23976*(d-1)+575424*(b-1)+n1
n2_location<-999*(g-1)+23976*(e-1)+575424*(c-1)+n2

#Eliminating number plates that do not start with K and
#Eliminating the alphabets not included in Kenyan Number plate, that is, I and O

if((n_1[1]!="K")||(n_1[2]=="I")||(n_1[3]=="I")||(n_1[7]=="I")||(n_1[2]=="O")||(n_1[3]=="O")||(n_1[7]=="O")||(n_2[1]!="K")||(n_2[2]=="I")||(n_2[3]=="I")||(n_2[7]=="I")||(n_2[2]=="O")||(n_2[3]=="O")||(n_2[7]=="O")){
cars_bought<-"NOT APPLICABLE!"
}else{
cars_bought<-n2_location-n1_location+1
}

return(cars_bought)
}

cars(KAA999Z,KAB001A)
