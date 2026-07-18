/* Smoke test PostgreSQL through the 64-bit PostgreSQL Unicode ODBC driver. */
#include "hbclass.ch"

PROCEDURE Main()
   LOCAL cHost := hb_GetEnv( "TOKO_PG_HOST" )
   LOCAL cDb   := hb_GetEnv( "TOKO_PG_DATABASE" )
   LOCAL cUser := hb_GetEnv( "TOKO_PG_USER" )
   LOCAL cPass := hb_GetEnv( "TOKO_PG_PASSWORD" )
   LOCAL oConn
   LOCAL oRs

   IF Empty( cDb )
      cDb := "toko"
   ENDIF
   IF Empty( cUser )
      cUser := "toko_app"
   ENDIF

   IF Empty( cHost ) .OR. Empty( cPass )
      ? "Set TOKO_PG_HOST dan TOKO_PG_PASSWORD terlebih dahulu."
      RETURN
   ENDIF

   oConn := win_OleCreateObject( "ADODB.Connection" )
   oConn:ConnectionString := ;
      "Driver={PostgreSQL Unicode(x64)};Server=" + cHost + ;
      ";Port=5432;Database=" + cDb + ";Uid=" + cUser + ;
      ";Pwd=" + cPass + ";SSLmode=require;"
   oConn:Open()
   oRs := oConn:Execute( "SELECT current_database() AS database_name" )
   ? "Terhubung ke PostgreSQL:", oRs:Fields( "database_name" ):Value
   oRs:Close()
   oConn:Close()
RETURN
