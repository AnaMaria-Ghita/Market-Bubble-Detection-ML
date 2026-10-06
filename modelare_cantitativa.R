library(openxlsx)
library(ggplot2)
library(dplyr)
library(tidyr)
library(moments)
library(tseries)
library(exuber)


GSPC <- read.xlsx("GSPC_date.xlsx")
IXIC <- read.xlsx("IXIC_date.xlsx")
GLD <- read.xlsx("GLD_date.xlsx")
USO <- read.xlsx("USO_date.xlsx")
ETH <- read.xlsx("ETH-USD_date.xlsx")


# data cleaning -----------------------------------------------------------

sum(is.na(GSPC$adjusted))
sum(is.na(IXIC$adjusted))
sum(is.na(GLD$adjusted))
sum(is.na(USO$adjusted))
sum(is.na(ETH$adjusted))


# Transformare logaritmica

GSPC$log_close <- log(GSPC$adjusted)
IXIC$log_close <- log(IXIC$adjusted)
GLD$log_close  <- log(GLD$adjusted)
USO$log_close  <- log(USO$adjusted)
ETH$log_close  <- log(ETH$adjusted)

# randamente logaritmice

GSPC$ret <- c(NA, diff(GSPC$log_close))
IXIC$ret <- c(NA, diff(IXIC$log_close))
GLD$ret  <- c(NA, diff(GLD$log_close))
USO$ret  <- c(NA, diff(USO$log_close))
ETH$ret  <- c(NA, diff(ETH$log_close))


# statistici descriptive

desc_stats <- function(serie, nume) {
  serie <- na.omit(serie)
  jb    <- jarque.bera.test(serie)
  data.frame(
    Activ             = nume,
    Medie             = round(mean(serie), 4),
    Mediana           = round(median(serie), 4),
    Minim             = round(min(serie), 4),
    Maxim             = round(max(serie), 4),
    Dev.Standard      = round(sd(serie), 4),
    Skewness          = round(skewness(serie), 4),
    Kurtosis          = round(kurtosis(serie), 4),
    Jarque.Bera       = round(jb$statistic, 4),
    Probabilitate     = round(jb$p.value, 4)
  )
}


tabel_desc <- rbind(
  desc_stats(GSPC$log_close, "S&P 500 (^GSPC)"),
  desc_stats(IXIC$log_close, "NASDAQ (^IXIC)"),
  desc_stats(GLD$log_close,  "Aur (GLD)"),
  desc_stats(USO$log_close,  "Petrol (USO)"),
  desc_stats(ETH$log_close,  "Ethereum (ETH-USD)")
)

print(tabel_desc)

# trend general


png("Figura1_preturi_absolute.png", width = 1200, height = 800, res = 120)
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(GSPC$date, GSPC$adjusted, type = "l", col = "blue",
     main = "S&P 500 (^GSPC)", xlab = "Data", ylab = "Pret ajustat")

plot(IXIC$date, IXIC$adjusted, type = "l", col = "red",
     main = "NASDAQ (^IXIC)", xlab = "Data", ylab = "Pret ajustat")

plot(GLD$date, GLD$adjusted, type = "l", col = "darkgoldenrod",
     main = "Aur (GLD)", xlab = "Data", ylab = "Pret ajustat")

plot(USO$date, USO$adjusted, type = "l", col = "darkblue",
     main = "Petrol (USO)", xlab = "Data", ylab = "Pret ajustat")

plot(ETH$date, ETH$adjusted, type = "l", col = "orange",
     main = "Ethereum (ETH-USD)", xlab = "Data", ylab = "Pret ajustat")
dev.off()



png("Figura2_preturi_logaritmice.png", width = 1200, height = 800, res = 120)
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(GSPC$date, GSPC$log_close, type = "l", col = "blue",
     main = "S&P 500 (^GSPC)", xlab = "Data", ylab = "Log Pret")

plot(IXIC$date, IXIC$log_close, type = "l", col = "red",
     main = "NASDAQ (^IXIC)", xlab = "Data", ylab = "Log Pret")

plot(GLD$date, GLD$log_close, type = "l", col = "darkgoldenrod",
     main = "Aur (GLD)", xlab = "Data", ylab = "Log Pret")

plot(USO$date, USO$log_close, type = "l", col = "darkblue",
     main = "Petrol (USO)", xlab = "Data", ylab = "Log Pret")

plot(ETH$date, ETH$log_close, type = "l", col = "orange",
     main = "Ethereum (ETH-USD)", xlab = "Data", ylab = "Log Pret")
dev.off()




# TESTUL ADF --------------------------------------------------------------


library(tseries)

#install.packages("urca")

library(urca)

adf_test_urca <- function(serie, nume) {
  serie <- na.omit(serie)
  
  # NIVEL
  # Cu intercept
  adf_nivel_i  <- ur.df(serie, type = "drift", selectlags = "AIC")
  
  # Cu trend + intercept
  adf_nivel_ti <- ur.df(serie, type = "trend", selectlags = "AIC")
  
  # DIFERENTA
  dserie <- diff(serie)
  dserie <- na.omit(dserie)
  
  adf_diff_i  <- ur.df(dserie, type = "drift", selectlags = "AIC")
  adf_diff_ti <- ur.df(dserie, type = "trend", selectlags = "AIC")
  
  stat_nivel_i  <- adf_nivel_i@teststat[1]
  stat_nivel_ti <- adf_nivel_ti@teststat[1]
  stat_diff_i   <- adf_diff_i@teststat[1]
  stat_diff_ti  <- adf_diff_ti@teststat[1]
  
  crit_nivel_i  <- adf_nivel_i@cval[1, "5pct"]
  crit_nivel_ti <- adf_nivel_ti@cval[1, "5pct"]
  crit_diff_i   <- adf_diff_i@cval[1, "5pct"]
  crit_diff_ti  <- adf_diff_ti@cval[1, "5pct"]
  
  
  p_nivel_i  <- ifelse(stat_nivel_i < crit_nivel_i, "Stationar", "Nest")
  p_nivel_ti <- ifelse(stat_nivel_ti < crit_nivel_ti, "Stationar", "Nest")
  p_diff_i   <- ifelse(stat_diff_i < crit_diff_i, "Stationar", "Nest")
  p_diff_ti  <- ifelse(stat_diff_ti < crit_diff_ti, "Stationar", "Nest")
  
  data.frame(
    Activ = nume,
    
    ADF_Nivel_Intercept = round(stat_nivel_i, 4),
    Crit_5pct_Nivel_I   = crit_nivel_i,
    Rezultat_Nivel_I    = p_nivel_i,
    
    ADF_Nivel_Trend     = round(stat_nivel_ti, 4),
    Crit_5pct_Nivel_TI  = crit_nivel_ti,
    Rezultat_Nivel_TI   = p_nivel_ti,
    
    ADF_Diff_Intercept  = round(stat_diff_i, 4),
    Crit_5pct_Diff_I    = crit_diff_i,
    Rezultat_Diff_I     = p_diff_i,
    
    ADF_Diff_Trend      = round(stat_diff_ti, 4),
    Crit_5pct_Diff_TI   = crit_diff_ti,
    Rezultat_Diff_TI    = p_diff_ti,
    
    Ordin_Integrare = ifelse(p_diff_i == "Stationar", "I(1)", "I(0)")
  )
}

tabel_adf <- rbind(
  adf_test_urca(GSPC$log_close, "S&P 500 (^GSPC)"),
  adf_test_urca(IXIC$log_close, "NASDAQ (^IXIC)"),
  adf_test_urca(GLD$log_close,  "Aur (GLD)"),
  adf_test_urca(USO$log_close,  "Petrol (USO)"),
  adf_test_urca(ETH$log_close,  "Ethereum (ETH-USD)")
)

rownames(tabel_adf) <- NULL
print(tabel_adf)

write.xlsx(tabel_adf, "tabel_adf.xlsx", overwrite = TRUE)

# TEST KPSS
library(tseries)

kpss_test <- function(serie, nume) {
  serie <- na.omit(serie)
  
  # Test la nivel - cu nivel (mu)
  kpss_nivel_mu   <- kpss.test(serie, null = "Level")
  
  # Test la nivel - cu trend (tau)
  kpss_nivel_tau  <- kpss.test(serie, null = "Trend")
  
  # Test la prima diferenta - cu nivel
  kpss_diff_mu    <- kpss.test(diff(serie), null = "Level")
  
  # Test la prima diferenta - cu trend
  kpss_diff_tau   <- kpss.test(diff(serie), null = "Trend")
  
  data.frame(
    Activ                        = nume,
    KPSS_Nivel_Nivel             = round(kpss_nivel_mu$statistic, 4),
    Prob_Nivel_Nivel             = round(kpss_nivel_mu$p.value, 4),
    Stationaritate_Nivel_Nivel   = ifelse(kpss_nivel_mu$p.value < 0.05, 
                                          "Nestaționar", "Staționar"),
    KPSS_Nivel_Trend             = round(kpss_nivel_tau$statistic, 4),
    Prob_Nivel_Trend             = round(kpss_nivel_tau$p.value, 4),
    Stationaritate_Nivel_Trend   = ifelse(kpss_nivel_tau$p.value < 0.05, 
                                          "Nestaționar", "Staționar"),
    KPSS_Diff_Nivel              = round(kpss_diff_mu$statistic, 4),
    Prob_Diff_Nivel              = round(kpss_diff_mu$p.value, 4),
    Stationaritate_Diff_Nivel    = ifelse(kpss_diff_mu$p.value > 0.05, 
                                          "Staționar", "Nestaționar"),
    KPSS_Diff_Trend              = round(kpss_diff_tau$statistic, 4),
    Prob_Diff_Trend              = round(kpss_diff_tau$p.value, 4),
    Stationaritate_Diff_Trend    = ifelse(kpss_diff_tau$p.value > 0.05, 
                                          "Staționar", "Nestaționar"),
    Ordin_Integrare              = ifelse(kpss_diff_mu$p.value > 0.05, "I(1)", "I(0)")
  )
}

tabel_kpss <- rbind(
  kpss_test(GSPC$log_close, "S&P 500 (^GSPC)"),
  kpss_test(IXIC$log_close, "NASDAQ (^IXIC)"),
  kpss_test(GLD$log_close,  "Aur (GLD)"),
  kpss_test(USO$log_close,  "Petrol (USO)"),
  kpss_test(ETH$log_close,  "Ethereum (ETH-USD)")
)

rownames(tabel_kpss) <- NULL
print(tabel_kpss)
write.xlsx(tabel_kpss, "tabel_kpss.xlsx", overwrite = TRUE)


# Exponential Curve Fitting -----------------------------------------------

install.packages("remotes")
install.packages("exuber")
install.packages("gridExtra")

library(exuber)
library(tseries)
library(zoo)
library(ggplot2)
library(gridExtra) 

date_cal_clean <- as.Date(prețuri <- as.numeric(GSPC$date), origin = "1899-12-30")


# DEFINIRE FUNCTIE run_excf_manual
run_excf_manual <- function(pret, window_size = 150) {
  n <- length(pret)
  omega <- rep(NA, n)
  
  for (i in (window_size + 1):n) {
    fereastra <- pret[(i - window_size):i]
    P0 <- fereastra[1]
    y <- fereastra - P0
    
    if (any(is.na(y)) || sd(y) == 0) next
    
    tryCatch({
      #  modelul AR(1) fara intercept
      y_lag <- y[-length(y)]
      y_cur <- y[-1]
      
      # Regresia simpla fara intercept
      omega_est <- sum(y_lag * y_cur) / sum(y_lag^2)
      omega[i] <- omega_est
    }, error = function(e) NULL)
  }
  
  return(omega)
}

plot_excf_articol_profesional <- function(data_obj, nume_activ, date_vector) {

  pret <- as.numeric(na.omit(data_obj$adjusted))
  n <- length(pret)
  omega <- run_excf_manual(pret, window_size = 150) 
  
  df_plot <- data.frame(
    Date = date_vector[1:n], 
    Price = pret, 
    Omega = omega
  )
  
  raw_bubble <- ifelse(!is.na(df_plot$Omega) & df_plot$Omega > 1, 1, 0)
  
  # rolling mean pentru a vedea dacă bula persista
  persistență <- zoo::rollmean(raw_bubble, k = 5, fill = 0, align = "right")
  df_plot$is_bubble_filtered <- ifelse(persistență == 1, 1, 0)
  
  p1 <- ggplot(df_plot, aes(x = Date, y = Price)) +
    geom_rect(aes(xmin = Date, xmax = Date + 1, ymin = -Inf, ymax = Inf, 
                  alpha = as.factor(is_bubble_filtered)), 
              fill = "pink", color = NA, show.legend = FALSE) +
    scale_alpha_manual(values = c("0" = 0, "1" = 0.5)) +
    geom_line(color = "black", size = 0.5) +
    labs(title = paste("Metoda EXCF -", nume_activ), x = "", y = "Preț") +
    theme_minimal()
  
  # Graficul Omega
  p2 <- ggplot(df_plot, aes(x = Date, y = Omega)) +
    geom_line(size = 0.4) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    labs(x = "Anul", y = "omega(i, Ti)") +
    theme_minimal()
  
  grid.arrange(p1, p2, heights = c(2, 1))
}

#GSPC

f1_data <- GSPC[date_cal_clean >= "2015-01-01" & date_cal_clean <= "2017-12-31", ]
f2_data <- GSPC[date_cal_clean >= "2018-01-01" & date_cal_clean <= "2020-12-31", ]
f3_data <- GSPC[date_cal_clean >= "2021-01-01", ]

f1_date <- date_cal_clean[date_cal_clean >= "2015-01-01" & date_cal_clean <= "2017-12-31"]
f2_date <- date_cal_clean[date_cal_clean >= "2018-01-01" & date_cal_clean <= "2020-12-31"]
f3_date <- date_cal_clean[date_cal_clean >= "2021-01-01"]

plot_excf_articol_profesional(f1_data, "S&P 500 (Regim Stabil)", f1_date)
plot_excf_articol_profesional(f2_data, "S&P 500 (Tranziție & Pandemie)", f2_date)
plot_excf_articol_profesional(f3_data, "S&P 500 (Inflație & AI Era)", f3_date)

# IXIC

f1_data <- IXIC[date_cal_clean >= "2015-01-01" & date_cal_clean <= "2017-12-31", ]
f2_data <- IXIC[date_cal_clean >= "2018-01-01" & date_cal_clean <= "2020-12-31", ]
f3_data <- IXIC[date_cal_clean >= "2021-01-01", ]

f1_date <- date_cal_clean[date_cal_clean >= "2015-01-01" & date_cal_clean <= "2017-12-31"]
f2_date <- date_cal_clean[date_cal_clean >= "2018-01-01" & date_cal_clean <= "2020-12-31"]
f3_date <- date_cal_clean[date_cal_clean >= "2021-01-01"]

plot_excf_articol_profesional(f1_data, "NASDAQ (Regim Stabil)", f1_date)
plot_excf_articol_profesional(f2_data, "NASDAQ (Tranziție & Pandemie)", f2_date)
plot_excf_articol_profesional(f3_data, "NASDAQ (Inflație & AI Era)", f3_date)


# ETH

f1_data <- ETH[date_cal_clean >= "2015-01-01" & date_cal_clean <= "2017-12-31", ]
f2_data <- ETH[date_cal_clean >= "2018-01-01" & date_cal_clean <= "2020-12-31", ]
f3_data <- ETH[date_cal_clean >= "2021-01-01", ]

f1_date <- date_cal_clean[date_cal_clean >= "2015-01-01" & date_cal_clean <= "2017-12-31"]
f2_date <- date_cal_clean[date_cal_clean >= "2018-01-01" & date_cal_clean <= "2020-12-31"]
f3_date <- date_cal_clean[date_cal_clean >= "2021-01-01"]

plot_excf_articol_profesional(f1_data, "ETH (Regim Stabil)", f1_date)
plot_excf_articol_profesional(f2_data, "ETH (Tranziție & Pandemie)", f2_date)
plot_excf_articol_profesional(f3_data, "ETH (Inflație & AI Era)", f3_date)

# GLD

f1_data <- GLD[date_cal_clean >= "2015-01-01" & date_cal_clean <= "2017-12-31", ]
f2_data <- GLD[date_cal_clean >= "2018-01-01" & date_cal_clean <= "2020-12-31", ]
f3_data <- GLD[date_cal_clean >= "2021-01-01", ]

f1_date <- date_cal_clean[date_cal_clean >= "2015-01-01" & date_cal_clean <= "2017-12-31"]
f2_date <- date_cal_clean[date_cal_clean >= "2018-01-01" & date_cal_clean <= "2020-12-31"]
f3_date <- date_cal_clean[date_cal_clean >= "2021-01-01"]

plot_excf_articol_profesional(f1_data, "GLD (Regim Stabil)", f1_date)
plot_excf_articol_profesional(f2_data, "GLD (Tranziție & Pandemie)", f2_date)
plot_excf_articol_profesional(f3_data, "GLD (Inflație & AI Era)", f3_date)

# uso 

f1_data <- USO[date_cal_clean >= "2015-01-01" & date_cal_clean <= "2017-12-31", ]
f2_data <- USO[date_cal_clean >= "2018-01-01" & date_cal_clean <= "2020-12-31", ]
f3_data <- USO[date_cal_clean >= "2021-01-01", ]


f1_date <- date_cal_clean[date_cal_clean >= "2015-01-01" & date_cal_clean <= "2017-12-31"]
f2_date <- date_cal_clean[date_cal_clean >= "2018-01-01" & date_cal_clean <= "2020-12-31"]
f3_date <- date_cal_clean[date_cal_clean >= "2021-01-01"]


plot_excf_articol_profesional(f1_data, "USO (Regim Stabil)", f1_date)
plot_excf_articol_profesional(f2_data, "USO (Tranziție & Pandemie)", f2_date)
plot_excf_articol_profesional(f3_data, "USO (Inflație & AI Era)", f3_date)


# TEST GSADF 
plot_gsadf_manual <- function(serie, nume, date_vector) {
  serie <- as.numeric(na.omit(serie))
  
  if (!inherits(date_vector, "Date")) {
    date_vector <- as.Date(as.numeric(date_vector), origin = "1899-12-30")
  }
  
  date_vector <- date_vector[1:length(serie)]

  rezultat <- radf(serie)
  cv       <- radf_mc_cv(length(serie))
  
  # minw din atribute
  min_win <- attr(rezultat, "minw")
  
  # secventa BSADF
  bsadf_seq <- as.numeric(rezultat$bsadf)
  
  date_bsadf <- date_vector[(min_win + 1):length(serie)]
  
  df_plot <- data.frame(
    date  = date_bsadf,
    bsadf = bsadf_seq
  )
  
  # Valoarea critica la 95%
  cv_95 <- cv$bsadf_cv[2]
  
  #  perioadele de bula
  df_plot$bula <- ifelse(df_plot$bsadf > cv_95, 1, 0)

  ggplot(df_plot, aes(x = date)) +
    geom_rect(aes(xmin = date, xmax = date + 1,
                  ymin = -Inf, ymax = Inf,
                  alpha = as.factor(bula)),
              fill = "pink", color = NA, show.legend = FALSE) +
    scale_alpha_manual(values = c("0" = 0, "1" = 0.5)) +
    geom_line(aes(y = bsadf), color = "black", size = 0.5) +
    geom_hline(yintercept = cv_95, color = "red",
               linetype = "dashed", size = 0.7) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(title = paste("Metoda GSADF -", nume),
         x = "Data", y = "BSADF") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# 1. S&P 500
png("GSADF_GSPC.png", width = 1400, height = 900, res = 150)
p1 <- plot_gsadf_manual(GSPC$log_close, "S&P 500 (^GSPC)", GSPC$date)
print(p1)
dev.off()

# 2. NASDAQ
png("GSADF_IXIC.png", width = 1400, height = 900, res = 150)
p2 <- plot_gsadf_manual(IXIC$log_close, "NASDAQ (^IXIC)", IXIC$date)
print(p2)
dev.off()

# 3. Aur
png("GSADF_GLD.png", width = 1400, height = 900, res = 150)
p3 <- plot_gsadf_manual(GLD$log_close, "Aur (GLD)", GLD$date)
print(p3)
dev.off()

# 4. Petrol
png("GSADF_USO.png", width = 1400, height = 900, res = 150)
p4 <- plot_gsadf_manual(USO$log_close, "Petrol (USO)", USO$date)
print(p4)
dev.off()

# 5. Ethereum
png("GSADF_ETH.png", width = 1400, height = 900, res = 150)
p5 <- plot_gsadf_manual(ETH$log_close, "Ethereum (ETH-USD)", ETH$date)
print(p5)
dev.off()

# Tabelul centralizator de datare a bulelor
tabel_datare_bule <- function(serie, nume, date_vector) {
  
  serie <- as.numeric(na.omit(serie))
  
  if (!inherits(date_vector, "Date")) {
    date_vector <- as.Date(as.numeric(date_vector), origin = "1899-12-30")
  }
  date_vector <- date_vector[1:length(serie)]
  rezultat <- radf(serie)
  cv <- radf_mc_cv(length(serie))
  
  min_win <- attr(rezultat, "minw")
  bsadf_seq <- as.numeric(rezultat$bsadf)
  date_bsadf <- date_vector[(min_win + 1):length(serie)]
  
  # Scalar fix la 95%
  cv_95 <- as.numeric(cv$bsadf_cv[2])
  
  este_bula <- bsadf_seq > cv_95
  
  episoade_list <- list()
  in_bula <- FALSE
  start_idx <- NULL
  
  for (i in seq_along(este_bula)) {
    if (este_bula[i] && !in_bula) {
      in_bula <- TRUE
      start_idx <- i
    } else if (!este_bula[i] && in_bula) {
      in_bula <- FALSE
      end_idx <- i - 1
      durata <- as.numeric(date_bsadf[end_idx] - date_bsadf[start_idx])
      if (durata >= 5) {
        episoade_list[[length(episoade_list) + 1]] <- data.frame(
          Activ = nume,
          Inceput = date_bsadf[start_idx],
          Sfarsit = date_bsadf[end_idx],
          Durata_Zile = durata
        )
      }
    }
  }
  
  if (in_bula) {
    end_idx <- length(este_bula)
    durata <- as.numeric(date_bsadf[end_idx] - date_bsadf[start_idx])
    if (durata >= 5) {
      episoade_list[[length(episoade_list) + 1]] <- data.frame(
        Activ = nume,
        Inceput = date_bsadf[start_idx],
        Sfarsit = date_bsadf[end_id],
        Durata_Zile = durata
      )
    }
  }
  
  if (length(episoade_list) > 0) {
    return(do.call(rbind, episoade_list))
  } else {
    return(data.frame(
      Activ = nume, Inceput = NA, Sfarsit = NA, Durata_Zile = NA
    ))
  }
}
active_list <- list(
  list(data = GSPC, label = "S&P 500"),
  list(data = IXIC, label = "NASDAQ"),
  list(data = GLD, label = "Aur (GLD)"),
  list(data = USO, label = "Petrol (USO)"),
  list(data = ETH, label = "Ethereum")
)
tabel_final <- do.call(rbind, lapply(active_list, function(x) {
  tabel_datare_bule(x$data$log_close, x$label, x$data$date)
}))
tabel_final <- na.omit(tabel_final)
rownames(tabel_final) <- NULL
write.csv(tabel_final, "Tabel_Datare_Bule_GSADF.csv", row.names = FALSE)
print(tabel_final)

  
