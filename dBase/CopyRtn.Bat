@Echo Off
Echo. BackUp Data Rutin ke C:\Toko.Bak
Echo. --------------------------------
Md C:\Toko.Bak
Copy Z:*.Exe C:\Toko.Bak /y
Copy Z:*.Bat C:\Toko.Bak /y
Copy Z:*.Set C:\Toko.Bak /y
Copy Z:Stok.Dta  C:\Toko.Bak /y
Copy Z:Stok*.Ndx C:\Toko.Bak /y
Copy Z:Jenis*.*  C:\Toko.Bak /y
Copy Z:Supp*.*   C:\Toko.Bak /y
Copy Z:Kasir.Dta C:\Toko.Bak /y
Copy Z:Jual.Dta  C:\Toko.Bak /y
Copy Z:Kas.Dta   C:\Toko.Bak /y
Copy Z:Posaku*.* C:\Toko.Bak /y
Echo.
Echo. BackUp Selesai.
