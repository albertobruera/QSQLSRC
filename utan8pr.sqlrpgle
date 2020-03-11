      **************************************************************************
      * Modifica effettuata da: Alberto Bruera
      * In data               : 11/06/2019
      * Id modifica           : ab001
      * Desc. modifica        : Da UTAN8PR aggiunta scrittura file $$MOVINS
      *                         con reperimento valori da CGSPA01L e ANAGRA
      **************************************************************************
      * Modifica effettuata da: Alberto Bruera
      * In data               : 24/07/2019
      * Id modifica           : ab002
      * Desc. modifica        : Modifica reperimento insoluti su CGSPA00F
      *                         per avere corrispondenza con dati JGalileo
      *                         eliminato ciclo rpg su ANAGRA
      **************************************************************************
      * Modifica effettuata da: Alberto Bruera
      * In data               : 29/08/2019
      * Id modifica           : ab003
      * Desc. modifica        : Aggiunta valorizzazione campo MDARIF su $$MOVINS
      **************************************************************************
      * Modifica effettuata da: Alberto Bruera
      * In data               : 06/03/2019
      * Id modifica           : ab004
      * Desc. modifica        : Modificato SQL per reperimento insoluti
      **************************************************************************
     H DecEdit('0,') DatEdit(*Dmy/) AlWNULL(*INPUTONLY)
     h fixnbr(*zoned:*INPUTPACKED)  Option(*NoDebugIo)
     F$$MOVINS  O  a E             DISK
     d Ds_Insoluti     ds                  Qualified
     d   CodCli                       5s 0
     d   Zona                         5s 0
     d   NrPart                       6s 0
     d   DtReg                        8s 0
     d   DtDoc                        8s 0
     d   DtSca                        8s 0
     d   Imp                         13p 2
      *Clear flag insoluti
       Exec Sql
         Update anagra set anv4 = ' ';
      *

ab004  Exec Sql
ab004    Declare INS cursor for
ab004      with partap00f as (select contcm, aaopcm, nparcm, sum(impocm)
ab004      from vig90dat.cgmov00f where contcm like '1003%'
ab004      group by contcm, aaopcm, nparcm
ab004      having sum(impocm) <> 0 )
ab004      select a.ancod, a.anzo, m.nparcm, m.dtrgcm, m.dt01cm,
ab004                     m.dt1scm, m.impocm
ab004       from vig90dat.cgmov00f m join partap00f pa on pa.contcm = m.contcm
ab004                                                 and pa.aaopcm = m.aaopcm
ab004                                                 and pa.nparcm = m.nparcm
ab004      join vig90dat.smtabca00l on causcm = k_cod
ab004      join qs36f.anagra a on '10030' || a.ancod = m.contcm
ab004      where tecfit = 'S ' ;
       Exec sql
ab002    Open INS;

ab002   Exec Sql
ab002    Fetch INS Into :Ds_Insoluti;
ab002   DoW SqlStt = '00000';
      ** //Scrive record per insoluti
ab002      MZONA = Ds_Insoluti.Zona;
ab002      MCLIX = Ds_Insoluti.CodCli;
ab002      TOT   = Ds_Insoluti.Imp      ;
ab002      MDAREX = Ds_Insoluti.DtReg ;
ab002      MNURIX = Ds_Insoluti.NrPart ;
ab002      MDASCX = Ds_Insoluti.DtSca ;
ab002      MDADOC = Ds_Insoluti.DtDoc ;
ab002      Write RMOVI;

ab002      Exec Sql
ab002        Update ANAGRA set ANV4 = 'I' where ANCOD = :Ds_Insoluti.CodCli;

ab002    Exec Sql
ab002    Fetch INS Into :Ds_Insoluti;
         EndDo;

ab002   Exec Sql
ab002    Close INS;

        *Inlr = *On;

