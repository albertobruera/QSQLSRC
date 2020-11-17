--  Generazione SQL
--  Versione:                  	V7R4M0 190621
--  Generata su:               	29/10/20 22:44:59
--  Database relazionale:      	B708A3F0
--  Opzioni standard:          	Db2 for i
CREATE OR REPLACE TABLE ALBERTODTA.FILLST00F (
	FL_NOME_LIBRERIA                     FOR COLUMN FL_LIB          CHAR(10) CCSID 280 NOT NULL DEFAULT '' ,
	FL_NOME_FILE                             FOR COLUMN FL_FILE         CHAR(10) CCSID 280 NOT NULL DEFAULT '' ,
	FL_NOME_CAMPO                        FOR COLUMN FL_CAMPO    CHAR(10) CCSID 280 NOT NULL DEFAULT '' ,
	FL_TIPO_DATO                            FOR COLUMN FL_TIPDAT    CHAR(10) CCSID 280 NOT NULL DEFAULT '' ,
	FL_LUNGHEZZA                            FOR COLUMN FL_LUNDAT   INTEGER NOT NULL DEFAULT 0 ,
	FL_CRITTOGRAFIA_CAMPO          		FOR COLUMN FL_CRITCAM  CHAR(1) CCSID 280 NOT NULL DEFAULT '' ,
	FL_LIBRERIA_FIELD_PROC_PGM 			FOR COLUMN FL_FPRLPGM    CHAR(10) CCSID 280 NOT NULL DEFAULT '' ,
	FL_NOME_PGM_FIELD_PROC      		FOR COLUMN FL_FPRPGM   CHAR(10) CCSID 280 NOT NULL DEFAULT '' ,
	FL_MASCHERA_CAMPO              		FOR COLUMN FL_MASCAM   CHAR(1) CCSID 280 NOT NULL DEFAULT '' ,
	FL_NOME_MASCHERA                  	FOR COLUMN FL_MASNOM  CHAR(256) CCSID 280 NOT NULL DEFAULT '' ,
	FL_NOME_UTENTE                      FOR COLUMN FL_UTENTE    CHAR(10) CCSID 280 NOT NULL DEFAULT '' ,
	FL_DESCRIZIONE_UTENTE          FOR COLUMN FL_DESUTE   CHAR(30) CCSID 280 NOT NULL DEFAULT '' ,
	CONSTRAINT ALBERTODTA.Q_ALBERTODTA_FILLST00F_FL_LIB_00001 PRIMARY KEY( FL_NOME_LIBRERIA , FL_NOME_FILE , FL_NOME_CAMPO , FL_NOME_UTENTE ) )
	
	RCDFMT RECFILLST  ;

ALTER TABLE ALBERTODTA.FILLST00F
	ADD CONSTRAINT ALBERTODTA.CST_LPGM_NPGM
	CHECK( ( FL_CRITTOGRAFIA_CAMPO = 'S' AND FL_LIBRERIA_FIELD_PROC_PGM <> ' ' AND FL_NOME_PGM_FIELD_PROC <> ' ' ) OR ( FL_CRITTOGRAFIA_CAMPO = 'N' AND FL_LIBRERIA_FIELD_PROC_PGM = ' ' AND FL_NOME_PGM_FIELD_PROC = ' ' )
                   OR ( FL_CRITTOGRAFIA_CAMPO = 'W' AND FL_LIBRERIA_FIELD_PROC_PGM <> ' ' AND FL_NOME_PGM_FIELD_PROC <> ' ' )) ;

ALTER TABLE ALBERTODTA.FILLST00F
	ADD CONSTRAINT ALBERTODTA.CST_MASCHERA_CAMPO
	CHECK( FL_MASCHERA_CAMPO = 'S' OR FL_MASCHERA_CAMPO = 'N' OR FL_MASCHERA_CAMPO = 'W') ;

ALTER TABLE ALBERTODTA.FILLST00F
	ADD CONSTRAINT ALBERTODTA.CST_NOME_LIBRERIA
	CHECK( FL_NOME_LIBRERIA <> ' ' ) ;

ALTER TABLE ALBERTODTA.FILLST00F
	ADD CONSTRAINT ALBERTODTA.CST_NOME_FILE
	CHECK( FL_NOME_FILE <> ' ' ) ;

ALTER TABLE ALBERTODTA.FILLST00F
	ADD CONSTRAINT ALBERTODTA.CST_NOME_CAMPO
	CHECK( FL_NOME_CAMPO <> ' ' ) ;

ALTER TABLE ALBERTODTA.FILLST00F
	ADD CONSTRAINT ALBERTODTA.CST_TIPO_DATO
	CHECK( FL_TIPO_DATO <> ' ' ) ;

ALTER TABLE ALBERTODTA.FILLST00F
	ADD CONSTRAINT ALBERTODTA.CST_LUNGHEZZA
	CHECK( FL_LUNGHEZZA <> 0 ) ;

ALTER TABLE ALBERTODTA.FILLST00F
	ADD CONSTRAINT ALBERTODTA.CST_LIBRERIA_FIELD_PROC_PGM
	CHECK( ( FL_CRITTOGRAFIA_CAMPO = 'S' AND FL_NOME_PGM_FIELD_PROC <> ' ' ) OR ( FL_CRITTOGRAFIA_CAMPO = 'N' AND FL_NOME_PGM_FIELD_PROC = ' ' )
			OR ( FL_CRITTOGRAFIA_CAMPO = 'W' AND FL_NOME_PGM_FIELD_PROC <> ' ' )) ;

ALTER TABLE ALBERTODTA.FILLST00F
	ADD CONSTRAINT ALBERTODTA.CST_NOME_PGM_FIELD_PROC
	CHECK( ( FL_CRITTOGRAFIA_CAMPO = 'S' AND FL_LIBRERIA_FIELD_PROC_PGM <> ' ' ) OR ( FL_CRITTOGRAFIA_CAMPO = 'N' AND FL_LIBRERIA_FIELD_PROC_PGM = ' ' )
			OR ( FL_CRITTOGRAFIA_CAMPO = 'W' AND FL_LIBRERIA_FIELD_PROC_PGM <> ' ' )) ;

ALTER TABLE ALBERTODTA.FILLST00F
	ADD CONSTRAINT ALBERTODTA.CST_NOME_MASCHERA
	CHECK( ( FL_MASCHERA_CAMPO = 'S' AND FL_NOME_MASCHERA <> ' ' ) OR ( FL_MASCHERA_CAMPO = 'N' AND FL_NOME_MASCHERA = ' ' ) ) ;

ALTER TABLE ALBERTODTA.FILLST00F
	ADD CONSTRAINT ALBERTODTA.CST_NOME_UTENTE
	CHECK( ( FL_MASCHERA_CAMPO = 'S' AND FL_NOME_UTENTE <> ' ' ) OR ( FL_MASCHERA_CAMPO = 'N' AND FL_NOME_UTENTE = ' ' ) ) ;

LABEL ON COLUMN ALBERTODTA.FILLST00F
( FL_NOME_LIBRERIA IS 'NOME LIBRERIA' ,
	FL_NOME_FILE IS 'NOME FILE' ,
	FL_NOME_CAMPO IS 'NOME CAMPO' ,
	FL_TIPO_DATO IS 'TIPO DATO' ,
	FL_LUNGHEZZA IS 'LUNGHEZZA CAMPO' ,
	FL_CRITTOGRAFIA_CAMPO IS 'CAMPO CRITTOGRAFATO S/N' ,
	FL_LIBRERIA_FIELD_PROC_PGM IS 'LIBRERIA PROG. FIELD PROC' ,
	FL_NOME_PGM_FIELD_PROC IS 'NOME PGM FIELD PROC' ,
	FL_MASCHERA_CAMPO IS 'CAMPO MASCHERATO S/N' ,
	FL_NOME_MASCHERA IS 'NOME MASCHERA' ,
	FL_NOME_UTENTE IS 'NOME UTENTE ABILITATO' ) ;

GRANT ALTER , DELETE , INDEX , INSERT , REFERENCES , SELECT , UPDATE
ON ALBERTODTA.FILLST00F TO ALBERTO WITH GRANT OPTION ;

GRANT DELETE , INSERT , SELECT , UPDATE
ON ALBERTODTA.FILLST00F TO PUBLIC ;

