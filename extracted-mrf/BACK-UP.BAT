@Echo off
Echo. Copy  Data  ke  Folder C:\STOK-OFF
Echo. Untuk Transaksi Kasir non Jaringan
Echo. Tekan ENTER=Proses    CTRL+C=Batal
Echo. ----------------------------------
Pause > nul
Copy *.Exe     C:\Stok-Off /Y
Copy *.Set     C:\Stok-Off /Y
Copy Stok.Dta  C:\Stok-Off /Y
Copy Stok*.Ndx C:\Stok-Off /Y
Copy Jual*.Dta C:\Stok-Off /Y
Copy Kasir.Dta C:\Stok-Off /Y
Copy Kas.Dta   C:\Stok-Off /Y
Echo.
Echo. Copy Data Selesai.  Tekan sembarang tombol...
Echo.
Pause > nul
