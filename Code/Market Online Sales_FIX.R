# Libraries
library (tidyverse)
library (dplyr)
library (ggplot2)

# Data
# Path file
file_path <- "Dataset/Online_Sales_Data.csv"

# Membaca file CSV
sales_data <- read.csv(file_path)

head (sales_data)
summary(sales_data)

# Clean Data
# 1.Check Duplicate Data
jmh_transaksi_unik <- n_distinct(sales_data$Transaction.ID) #n_distinct adalah fungsi yang sangat berguna untuk menghitung jumlah nilai unik dalam kolom tertentu.
print(jmh_transaksi_unik) # 240

jumlah_baris <- nrow(sales_data)
print(jumlah_baris) # 240

# Jika jumlah transaksi unik lebih kecil dari jumlah total 
# baris dalam sales_data, maka ada transaksi yang duplikat.

# 2.Kolom Bulan
sales_data <- sales_data %>%
  mutate(Month = format(as.Date(sales_data$Date), "%m"))
View(sales_data)

#sales_data <- subset(sales_data, select = -month)

# 3.Nilai Kosong
which(is.na(sales_data))
sum(is.na(sales_data)) # 0 

# Analisis
# 1.Bagaimana tren penjualan secara keseluruhan dari waktu ke waktu
sales_month <- sales_data %>% group_by (Month) %>%
  summarize(total_sales = sum(Total.Revenue))
print(sales_month)


ggplot(sales_month, aes(x = Month, y = total_sales, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_text(aes(label = total_sales), vjust = -0.5, hjust = 0.5, color = "black", size = 2) +  # Mengatur size menjadi 2 (ukuran lebih kecil)
  labs(title = "Tren Pendapatan Produk",
       x = "Bulan",
       y = "Total Pendapatan") +
  theme_minimal() #untuk mengatur tema plot agar terlihat lebih sederhana dengan latar belakang putih dan garis grid yang minim + 
  theme(axis.text.x = element_text(hjust = 1)) # untuk mengkustomisasi elemen-elemen spesifik dalam plot, dalam hal ini, sumbu x (axis.text.x). element_text(hjust = 1) mengatur posisi teks sumbu x ke arah kanan (horizontal justification). Pengaturan hjust = 1 berarti teks akan diposisikan di sebelah kanan titik atau label sumbu x, memastikan bahwa teksnya tidak tumpang tindih dan lebih mudah dibaca.

  
  # Menghitung jumlah produk terjual per bulan per kategori
  produk_terjual_per_bulan <- sales_data %>%
    group_by(Month) %>%
    summarize(total_terjual = sum(Units.Sold))
  
  # Menampilkan hasil
  print(produk_terjual_per_bulan)

  ggplot(produk_terjual_per_bulan, aes(x = Month, y = total_terjual, group = 1)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    geom_text(aes(label = total_terjual), vjust = -0.5, hjust = 0.5, color = "black", size = 2) +  # Mengatur size menjadi 2 (ukuran lebih kecil)
    labs(title = "Tren Pendapatan Produk",
         x = "Bulan",
         y = "Total Pendapatan") +
    labs(title = "Jumlah Produk Terjual per Bulan",
         x = "Bulan",
         y = "Total Produk Terjual") +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust = 1))

  sales_data_jan_feb <- sales_data %>%
    filter(Month %in% c("01", "02"))
  
  # Menghitung rata-rata harga per produk untuk bulan Januari dan Februari
  produk_jan_feb <- sales_data_jan_feb %>%
    group_by(Product.Name) %>%
    summarize(rata_rata_harga = mean(Unit.Price))
  
  # Menampilkan hasil
  View(produk_jan_feb)
  
  # 2.Produk dengan pendapatan tertinggi Top 5
  colnames(sales_data)
  
  # Menghitung total pendapatan per produk
  produk_summary <- sales_data %>%
    group_by(Product.Name) %>%
    summarize(total_pendapatan = sum(Total.Revenue)) %>%
    arrange(desc(total_pendapatan)) %>%
    slice_head(n = 5)
  
  # Menampilkan hasil
  View (produk_summary)
  
  ggplot(produk_summary, aes(x = reorder(Product.Name, -total_pendapatan), y = total_pendapatan)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = total_pendapatan), vjust = -0.5, color = "black", size = 3) +
    labs(title = "Total Pendapatan per Produk (Top 5)",
         x = "Produk",
         y = "Total Pendapatan") +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust = 1)) +
    coord_flip()
  
  # Menghitung total pendapatan per produk
  produk_summary2 <- sales_data %>%
    group_by(Product.Name) %>%
    summarize(total_pendapatan = sum(Total.Revenue),
              total_penjualan = sum(Units.Sold)) %>%
    arrange(desc(total_penjualan)) 
  
  # Menampilkan hasil
  View (produk_summary2)
  
  # Menghitung total pendapatan per produk
  produk_summary3 <- sales_data %>%
    group_by(Product.Category) %>%
    summarize(total_pendapatan = sum(Total.Revenue)) %>%
    arrange(desc(total_pendapatan)) 
  
  # Menampilkan hasil
  View (produk_summary3)
  
  ggplot(produk_summary3, aes(x = "", y = total_pendapatan, fill = Product.Category)) +
    geom_bar(stat = "identity", width = 2.5, color = "white") +  # Menambahkan width dan color
    geom_text(aes(label = paste0(round(total_pendapatan / sum(total_pendapatan) * 100), "%")),
              position = position_stack(vjust = 0.5), size = 2) +  # Menambahkan size
    coord_polar("y", start = 0) +
    labs(title = "Pendapatan per Kategori Produk",
         fill = "Kategori Produk") +
    theme_void() +
    theme(legend.position = "bottom",
          text = element_text(size = 7),  # Mengatur ukuran teks secara global
          plot.title = element_text(size = 9, face = "bold")) +  # Mengatur ukuran dan gaya judul
    scale_fill_brewer(palette = "Set3")  # Memilih skema warna

  # Menghitung total pendapatan per kategori
  produk_summary5 <- sales_data %>%
    group_by(Product.Category) %>%
    summarize(total_pendapatan_kategori = sum(Total.Revenue)) %>%
    arrange(desc(total_pendapatan_kategori)) 
  
  # Menampilkan hasil
  View (produk_summary5)
  
 
  ggplot(produk_summary5, aes(x = reorder(Product.Category, total_pendapatan_kategori), y = total_pendapatan_kategori, fill = Product.Category)) +
    geom_bar(stat = "identity", width = 0.7, color = "white") +
    geom_text(aes(label = paste0("$", total_pendapatan_kategori)), vjust = -0.5, size = 2) +  # Menambahkan label di atas bar
    labs(title = "Total Pendapatan per Kategori Produk",
         x = "Kategori Produk",
         y = "Total Pendapatan",
         fill = "Kategori Produk") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 5, hjust = 1),  # Mengatur ukuran dan orientasi teks sumbu x
          axis.title.x = element_text(size = 20),  # Mengatur ukuran judul sumbu x
          axis.text.y = element_text(size = 5),    # Mengatur ukuran teks sumbu y
          legend.position = "none")               # Menghilangkan legenda
  
  # 3.Pendapatan berdasarkan Region
  # Menghitung total pendapatan per kategori
  produk_region <- sales_data %>%
    group_by(Region) %>%
    summarize(total_pendapatan_region = sum(Total.Revenue)) %>%
    arrange(desc(total_pendapatan_region)) 
  
  View(produk_region)
  
  ggplot(produk_region, aes(x = reorder(Region, total_pendapatan_region), y = total_pendapatan_region, fill = Region)) +
    geom_bar(stat = "identity", color = "white") +
    geom_text(aes(label = paste0("$", total_pendapatan_region)), vjust = -0.5, size = 2) +  # Menambahkan label di atas bar
    labs(title = "Total Pendapatan per Region",
         x = "Region",
         y = "Total Pendapatan",
         fill = "Region") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 7, hjust = 1),  
          axis.title.x = element_text(size = 7),  
          axis.text.y = element_text(size = 9),    
          legend.position = "none")               