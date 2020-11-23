CREATE OR REPLACE FUNCTION CRYPTMSK0O/LIBSST ( P_STRING CHAR(21))
RETURNS CHAR(10)

  BEGIN
   DECLARE LIB CHAR(10);
   DECLARE POS INTEGER;

   SET POS = LOCATE('/', P_STRING);
   SET LIB = SUBSTRING(P_STRING, 1, POS-1);
   RETURN LIB;
  END;
