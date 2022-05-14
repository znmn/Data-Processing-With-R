# getwd()
datajam <- read.csv("data_uts.csv") # Memasukkan data dari file ke dalam variable

GroupedMean <- function(df){
  fis <- df$fi
  fixis <- df$fixi
  
  mean <- sum(fixis)/sum(fis)
  return(mean)
}

GroupedMedian <- function(df, p){
  b <- df[which.max(df$fi), "bkb"]
  n <- sum(df$fi)
  F <- df[which.max(df$fi)-1, "fi"]
  f <- df[which.max(df$fi), "fi"]
  
  median <- b + p * (((n/2)-F)/f)
  return(median)
}

GroupedModus <- function(df, p){
  b <- df[which.max(df$fi), "bkb"]
  b1 <- df[which.max(df$fi), "fi"]-df[which.max(df$fi)-1, "fi"]
  b2 <- df[which.max(df$fi), "fi"]-df[which.max(df$fi)+1, "fi"]
  
  modus <- b + p * (b1/(b1+b2))
  return(modus)
}

datajam$xi <- rowMeans(subset(datajam, select = c(jam_min, jam_max), na.rm = TRUE)) #Xi
datajam$fixi <- with(datajam, fi * xi) # fiXi

X <- GroupedMean(datajam) # Mean (X')

p <- datajam[1, "jam_max"] - datajam[1, "jam_min"] + 1 # Panjang Kelas Interval

datajam$bkb <- with(datajam, jam_min-0.5) # Batas Kelas Bawah
datajam$bka <- with(datajam, jam_max+0.5) # Batas Kelas Atas

Me <- GroupedMedian(datajam, p) # Median (Me)

Mo <- GroupedModus(datajam, p) # Modus (Mo)

R = max(datajam$xi)-min(datajam$xi) # Range (R)

#Simpangan rata-rata
fi <- c(16,47,23,16)

xi <- c(3,8,13,18)

simpanganrata <- 1/sum(fi)*sum(fi*abs(xi-X))

###

###Simpangan Baku
varian <- 1/sum(fi)*sum(fi*(xi-X)^2)

simpanganBaku <-sqrt(varian)

###

# Menampilkan Data dan Hasil Perhitungan
cat("[I] Data Lama Penggunaan HP Dalam Sehari Mahasiswa Fasilkom (Jam)\n")
cat("\n")
print(datajam)
cat("\n")
cat("[I] Mean : ", X, "\n")
cat("[I] Median : ", Me, "\n")
cat("[I] Modus : ", Mo, "\n")
cat("[I] Range : ", R, "\n")
cat("[I] Simpangan Rata-Rata: ",simpanganrata,"\n")
cat("[I] Simpangan Baku: ",simpanganBaku,"\n")

rm(list = ls()) # Bersihkan Global Environments

