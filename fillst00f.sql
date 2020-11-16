﻿--  Generazione SQL 
--  Versione:                  	V7R3M0 160422 
--  Generata su:               	06/11/20 09:39:54 
--  Database relazionale:      	S21ACA4V 
--  Opzioni standard:          	Db2 for i 
CREATE OR REPLACE TABLE ALBERTO.FILLST00F ( 
	FL_NOME_LIBRERIA FOR COLUMN FL_LIB     CHAR(10) CCSID 280 NOT NULL DEFAULT '' , 
	FL_NOME_FILE FOR COLUMN FL_FILE    CHAR(10) CCSID 280 NOT NULL DEFAULT '' , 
	FL_NOME_CAMPO FOR COLUMN FL_CAMPO   CHAR(10) CCSID 280 NOT NULL DEFAULT '' , 
	FL_TIPO_DATO FOR COLUMN FL_TIPDAT  CHAR(10) CCSID 280 NOT NULL DEFAULT '' , 
	FL_LUNGHEZZA FOR COLUMN FL_LUNDAT  INTEGER NOT NULL DEFAULT 0 , 
	FL_CRITTOGRAFIA_CAMPO FOR COLUMN FL_CRITCAM CHAR(1) CCSID 280 NOT NULL DEFAULT '' , 
	FL_LIBRERIA_FIELD_PROC_PGM FOR COLUMN FL_FPRLPGM  CHAR(10) CCSID 280 NOT NULL DEFAULT '' , 
	FL_NOME_PGM_FIELD_PROC FOR COLUMN FL_FPRPGM  CHAR(10) CCSID 280 NOT NULL DEFAULT '' , 
	FL_MASCHERA_CAMPO FOR COLUMN FL_MASCAM  CHAR(1) CCSID 280 NOT NULL DEFAULT '' , 
	FL_NOME_MASCHERA FOR COLUMN FL_MASNOM  CHAR(256) CCSID 280 NOT NULL DEFAULT '' , 
	FL_NOME_UTENTE FOR COLUMN FL_UTENTE  CHAR(10) CCSID 280 NOT NULL DEFAULT '' , 
	FL_DESCRIZIONE_UTENTE FOR COLUMN FL_DESUTE  CHAR(30) CCSID 280 NOT NULL DEFAULT '' )   
	  
	RCDFMT RECFILLST  ; 
  
ALTER TABLE ALBERTO.FILLST00F 
	ADD CONSTRAINT ALBERTO.CST_CRITTOGRAFIA_CAMPO 
	CHECK( FL_CRITTOGRAFIA_CAMPO = 'S' OR FL_CRITTOGRAFIA_CAMPO = 'N' ) ; 
  
ALTER TABLE ALBERTO.FILLST00F 
	ADD CONSTRAINT ALBERTO.CST_MASCHERA_CAMPO 
	CHECK( FL_MASCHERA_CAMPO = 'S' OR FL_MASCHERA_CAMPO = 'N' ) ; 
  
ALTER TABLE ALBERTO.FILLST00F 
	ADD CONSTRAINT ALBERTO.CST_NOME_LIBRERIA 
	CHECK( FL_NOME_LIBRERIA <> ' ' ) ; 
  
ALTER TABLE ALBERTO.FILLST00F 
	ADD CONSTRAINT ALBERTO.CST_NOME_FILE 
	CHECK( FL_NOME_FILE <> ' ' ) ; 
  
ALTER TABLE ALBERTO.FILLST00F 
	ADD CONSTRAINT ALBERTO.CST_NOME_CAMPO 
	CHECK( FL_NOME_CAMPO <> ' ' ) ; 
  
ALTER TABLE ALBERTO.FILLST00F 
	ADD CONSTRAINT ALBERTO.CST_TIPO_DATO 
	CHECK( FL_TIPO_DATO <> ' ' ) ; 
  
ALTER TABLE ALBERTO.FILLST00F 
	ADD CONSTRAINT ALBERTO.CST_LUNGHEZZA 
	CHECK( FL_LUNGHEZZA <> 0 ) ; 
  
ALTER TABLE ALBERTO.FILLST00F 
	ADD CONSTRAINT ALBERTO.CST_LIBRERIA_FIELD_PROC_PGM 
	CHECK( ( FL_CRITTOGRAFIA_CAMPO = 'S' AND FL_NOME_PGM_FIELD_PROC <> ' ' ) OR ( FL_CRITTOGRAFIA_CAMPO = 'N' AND FL_NOME_PGM_FIELD_PROC = ' ' ) ) ; 
  
ALTER TABLE ALBERTO.FILLST00F 
	ADD CONSTRAINT ALBERTO.CST_NOME_PGM_FIELD_PROC 
	CHECK( ( FL_CRITTOGRAFIA_CAMPO = 'S' AND FL_LIBRERIA_FIELD_PROC_PGM <> ' ' ) OR ( FL_CRITTOGRAFIA_CAMPO = 'N' AND FL_LIBRERIA_FIELD_PROC_PGM = ' ' ) ) ; 
  
ALTER TABLE ALBERTO.FILLST00F 
	ADD CONSTRAINT ALBERTO.CST_NOME_MASCHERA 
	CHECK( ( FL_MASCHERA_CAMPO = 'S' AND FL_NOME_MASCHERA <> ' ' ) OR ( FL_MASCHERA_CAMPO = 'N' AND FL_NOME_MASCHERA = ' ' ) ) ; 
  
ALTER TABLE ALBERTO.FILLST00F 
	ADD CONSTRAINT ALBERTO.CST_NOME_UTENTE 
	CHECK( ( FL_MASCHERA_CAMPO = 'N' AND FL_NOME_UTENTE = ' ' ) OR ( FL_MASCHERA_CAMPO = 'S' AND FL_NOME_UTENTE <> ' ' ) ) ; 
  
LABEL ON COLUMN ALBERTO.FILLST00F 
( FL_NOME_LIBRERIA IS 'NOME LIBRERIA' , 
	FL_NOME_FILE IS 'NOME FILE' , 
	FL_NOME_CAMPO IS 'NOME CAMPO' , 
	FL_TIPO_DATO IS 'TIPO DATO' , 
	FL_LUNGHEZZA IS 'LUNGHEZZA CAMPO' , 
	FL_CRITTOGRAFIA_CAMPO IS 'CAMPO CRITTOGRAFATO S/N/W' , 
	FL_LIBRERIA_FIELD_PROC_PGM IS 'LIBRERIA PROG. FIELD PROC' , 
	FL_NOME_PGM_FIELD_PROC IS 'NOME PGM FIELD PROC' , 
	FL_MASCHERA_CAMPO IS 'CAMPO MASCHERATO S/N' , 
	FL_NOME_MASCHERA IS 'NOME MASCHERA' , 
	FL_NOME_UTENTE IS 'NOME UTENTE ABILITATO' ) ; 
  
GRANT ALTER , DELETE , INDEX , INSERT , REFERENCES , SELECT , UPDATE   
ON ALBERTO.FILLST00F TO ALBERTO WITH GRANT OPTION ; 
  
GRANT DELETE , INSERT , SELECT , UPDATE   
ON ALBERTO.FILLST00F TO PUBLIC ; 
  
