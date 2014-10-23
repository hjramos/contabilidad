/*****************************GLBALAN2.P***************************************\
*   Autor : Rodolfo Garc¡a C rdenas (enero de 1995)                            *
* Objetivo: Listar el saldo  inicial, movimientos y saldo final de las cuentas *
*           por per¡odo.                                                       *
* Modif.  : EMS 01/98.  Cambio al formato del reporte.

  MODIF.  : LMMA AGOSTO 2000 CAMBIO A VERSION 9 (CALCULO DE CARGOS Y ABONOS)
  Modif.  : EMS-1 Sustituir etiquetas por incluidos (27 Feb 2003)
  Modif.  : EMS-2 Agregar manejo de Moneda Base (20 Mar 2003)

  Modif.  : Luis Manuel Maury Arcega 
  Fecha   : 28 de Junio 2005
  Objeto  : Manejo de multientidades 
  Clave   : LMMA-1
  Modif.  :
  Fecha   : 10/01/2014
  Objeto  : Generar archivo XML de acuerdo a los requerimientos del SAT Anexo 24
  Clave   : HSM
  
  Modif   : RMOD Abr 05, 2007. Sustituir etiquetas por incluidos.
\******************************************************************************/

def var nomsub as character no-undo.
def var nomcc  as character no-undo.
def var moneda like ac_curr format "x(3)" no-undo.
def var ctaca   as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var ctaab   as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var ctaab1  as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var ctacad  as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var ctaabd  as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var sdoito  as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var sdoitod as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var cato    as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var abto    as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var catod   as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var abtod   as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var ccab    as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var neto    as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var netod   as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var sdofto  as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var sdoftod as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var ctane   as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var ctaned  as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var sdoict  as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var sdoictd as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var sdofct  as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var sdofctd as decimal decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var sb_sdoict as dec decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var sb_ctaca as dec decimals 2  format "->>>>,>>>,>>>.99" no-undo.
def var sb_ctaab as dec decimals 2  format "->>>>,>>>,>>>.99" no-undo.
def var sb_ctane as dec decimals 2  format "->>>>,>>>,>>>.99" no-undo.
def var sb_sdofct as dec decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var sb_ctaned as dec decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var sb_sdoictd as dec decimals 2 format "->>>>,>>>,>>>.99" no-undo.
def var sb_sdofctd as dec decimals 2 format "->>>>,>>>,>>>.99" no-undo.

def var tipo like ac_mstr.ac_type no-undo.     /* EMS 01/96 */
def var ent like en_entity /*label "Entidad"*/ init "" 
                     /** LMMA-1 *** format "xx" ****/ no-undo.
def var per like acd_per /*label "Per¡odo"*/ format "99" no-undo.
def var an  like acd_year /*label "A¤o" */    format "9999" no-undo.
def var swsub as int no-undo.
def var fechai as date.
def var fechaf as date.
def var ctai  as char format "x(8)" 
              label {gl0077.i} /*"Cuenta "*/ init "" no-undo.
def var sctai as char format "x(8)" 
              label {gl0078.i} /*"SubCuenta "*/ init "" no-undo.
def var cci   as char format "x(8)" 
              label {gl0079.i} /*"Centro de Costo "*/ init ""   no-undo.
def var ctaf  as char format "x(8)" 
              label {gr0002.i} /*"A" */             init "" no-undo.
def var sctaf as char format "x(8)" 
              label {gr0002.i} /*"A" */             init "" no-undo.
def var ccf   as char format "x(8)" 
              label {gr0002.i} /*"A" */             init "" no-undo.
def var proyi like acd_project 
              label {gl0080.i} /*"Proyecto"*/   no-undo.
def var proyf like acd_project 
              label {gr0002.i} /*"A" */         no-undo.
def var ana as integer no-undo.
def var pera as integer no-undo.
def var wmndbase like gl_base_curr  no-undo.                  /** EMS-2 **/
def stream pant.


DEF VAR v_inclu    AS CHAR                             NO-UNDO. /* LMMA-1 */

define variable balanza as handle no-undo. /* HSM */
define variable xmlFilename as character no-undo. /* HSM */
define variable zipFilename as character no-undo. /* HSM */
define variable cuenta as character no-undo. /* HSM */
define variable totalctas as integer no-undo. /* HSM */

define variable hDoc as handle no-undo. /* HSM */
define variable hRoot as handle no-undo. /* HSM */
define variable hCtas as handle no-undo. /* HSM */

create x-document hDoc. /* HSM */
create x-noderef hRoot. /* HSM */
create x-noderef hCtas. /* HSM */

v_inclu = {gr0878.i}.                                           /* LMMA-1 */

{mfdtitle.i}

output stream pant to terminal.

ent = current_entity.  /** LMMA-1 **/

repeat:
      update  ent      colon 25 label {gr0094.i}
              validate(can-find(en_mstr where en_entity=input ent) 
                    OR INPUT ent = v_inclu,      /** LMMA-1 **/
              {gr0027.i} /*"Entidad Invalida"*/)    /** LMMA-1 **/
              HELP {gr0879.i}                       /** LMMA-1 **/
              an       colon 25 label {gr0024.i} 
              validate(an > 1993, {gr0044.i} /*"A¤o Inv lido"*/)
              per      colon 25 label {gr0023.i}
              validate(per > 0 and per < 13, {gr0010.i}
              /*"Per¡odo debe ser del 1 al 12" */)
              ctai     colon 25    ctaf  colon 60
              sctai    colon 25    sctaf colon 60
              cci      colon 25    ccf   colon 60
              with frame a width 80 side-labels.

              if  ctai = "" then  ctai = chr(32).
              if sctai = "" then sctai = chr(32).
              if   cci = "" then  cci  = chr(32).
              if  ctaf = "" then  ctaf = "zzzzzzzz".
              if sctaf = "" then sctaf = "zzzzzzzz".
              if   ccf = "" then   ccf = "zzzzzzzz".

              if per = 1 then assign pera = 12 ana = (an - 1).
              if per > 1 then assign pera = (per - 1)  ana = an.
			  
			  /* Inicia la generacion del archivo XML */
              xmlFilename = "HSM970930AU7" + string(an) + string(per,"99") + "BN.xml". /* HSM */
              zipFilename = "HSM970930AU7" + string(an) + string(per,"99") + "BN.zip". /* HSM */
			  
              hDoc:encoding = "UTF-8". /* HSM */
			  hDoc:create-node(hRoot,"BCE:Balanza","ELEMENT").  /* HSM */
			  hRoot:set-attribute("xmlns:BCE","http://www.sat.gob.mx/balanza"). /* HSM */
              hRoot:set-attribute("Version","1.0"). /* HSM */
              hRoot:set-attribute("RFC","HSM970930AU7"). /* HSM */
              /* El resto de los attributos se agregan al final del reporte */
  
  form header
       space(40)
       {gl0116.i} /*"BALANZA DE MOVIMIENTOS DEL PERIODO  "*/ 
       per {gr0138.i} /*"DE  "*/ an   skip
       space(111) 
       "<-------------- " {gl0117.i} /*MONEDA EXTRANJERA*/ " -------------->"
       skip
       {gl0079.i} /*"Centro de Costo"  space(16) */
       {gl0097.i} /*"Saldo Inicial" space(3) */
       at 28
       {gl0081.i} /*"C a r g o s"  space(11) */
       at 48
       {gl0082.i}  /*"A b o n o s" space(9) */
       at 62
       {gl0114.i}  /*"Saldo del Mes" SPACE(4) */
       at 79
       {gl0118.i}  /*"Saldo Acumulado" space(2) */
       at 96
       {gl0097.i}  /*"Saldo Inicial" space(3) */
       at 113
       {gl0030.i}  /*"Movimientos del mes"*/
       at 129
      /* space(1) */ 
       {gl0115.i}  /* "Saldo Final"  space(4) */
       at 149
       substr({gr0100.i},1,3) format "x(3)" /* "Mon" */
       at 164
       skip
       "-------------------------- ---------------- ----------------"
       "---------------- ---------------- ---------------- ----------------"
       "---------------- ---------------- ---" skip
       with frame enca page-top width 170.

  
  {rasprt.i "printer" 150}
  {cabeza.i}

  find first gl_ctrl no-lock no-error.                 /** EMS-2 **/
  assign wmndbase = gl_base_curr.                      /** EMS-2 **/
  

  /*  VALIDAR A¥O Y PERIODO CON EL CALENDARIO CONTABLE */
  find glc_cal where glc_year = an and glc_per = per no-lock no-error.
   if available glc_cal then do:
      fechai = glc_start.
      fechaf = glc_end.
  view frame enca.
    /* CICLO DE LAS CUENTAS QUE DEBEN INCLUIRSE EN EL REPORTE */
    for each asc_mstr where
                         asc_acc >= ctai   and
                         asc_acc <= ctaf   and
                         asc_sub >= sctai  and
                         asc_sub <= sctaf  and
                         asc_cc  >= cci    and
                         asc_cc  <= ccf
                         break by asc_acc by asc_sub by asc_cc
                         with frame x width 256 no-labels no-box:
    tipo = "".
    find ac_mstr where ac_code = asc_acc no-lock no-error.
      if available ac_mstr then assign tipo = ac_type         /* EMS 01/96 */
                                       moneda = ac_curr.
    find sb_mstr where sb_sub = asc_sub no-lock no-error.
      if available sb_mstr then nomsub = sb_desc.
    find cc_mstr where cc_ctr = asc_cc no-lock no-error.
      if available cc_mstr then nomcc = cc_desc.

    if first-of(asc_acc) then do:
       disp {gl0077.i} /*"cuenta"*/ asc_acc format "x(8)" ac_desc format "x(24)"
             when available ac_mstr
          with frame x1 width 256 no-labels no-box.
    /*   down with frame x1. */
    end.
    if first-of(asc_sub) then swsub = 0.

    /*** LMMA AGOSTO 2000 CAMBIO A VERSION 9 ****************/
    FOR EACH ACD_DET NO-LOCK USE-INDEX ACD_IND2
                               WHERE ACD_ACC    = ASC_ACC AND
                                     ACD_SUB    = ASC_SUB AND
                                     ACD_CC     = ASC_CC  AND
                                    (ACD_ENTITY = ENT 
        /** LMMA-1 **/               OR ent = v_inclu)  AND
                                     ACD_YEAR   <= AN:
                            
        IF ACD_YEAR = AN AND ACD_PER > PER THEN
           NEXT.

        /***** Inicia LMMA-1   Eliminacion para Consolidar ******/
        IF ent = v_inclu THEN DO:
          FIND agr_ctas NO-LOCK WHERE agr_enti = acd_entity AND
                                      agr_rep  = "ELIM"     AND
                                      agr_cta  = acd_acc    AND
                                      agr_sub  = acd_sub    AND
                                      agr_cc   = acd_cc NO-ERROR.
          IF AVAILABLE agr_ctas THEN
            NEXT.
        END.
        /***** Termina LMMA-1 ***************************************/    
        
        /***  CALCULA SALDO INICIAL ***/
        IF (ACD_YEAR < AN AND TIPO <> "E" AND TIPO <> "I")
           OR (ACD_YEAR = AN AND ACD_PER < PER) THEN
           ASSIGN SDOICT     = SDOICT + ACD_AMT
                  SDOICTD    = SDOICTD + ACD_CURR_AMT
                  SDOITO     = SDOITO + ACD_AMT
                  SDOITOD    = SDOITOD + ACD_CURR_AMT
                  SB_SDOICT  = SB_SDOICT + ACD_AMT
                  SB_SDOICTD = SB_SDOICTD + ACD_CURR_AMT.
                  
         /*** CALCULA SALDO FINAL ***/
        IF (ACD_YEAR < AN AND TIPO <> "E" AND TIPO <> "I")
            OR (ACD_YEAR = AN AND ACD_PER <= PER) THEN
              ASSIGN
                  SDOFCT     = SDOFCT + ACD_AMT
                  SDOFCTD    = SDOFCTD + ACD_CURR_AMT
                  SDOFTO     = SDOFTO + ACD_AMT
                  SDOFTOD    = SDOFTOD + ACD_CURR_AMT
                  SB_SDOFCT  = SB_SDOFCT + ACD_AMT
                  SB_SDOFCTD = SB_SDOFCTD + ACD_CURR_AMT.
           
        /**** CALCULA CARGOS, ABONOS Y NETOS ****/
        IF ACD_YEAR = AN AND ACD_PER = PER THEN
           ASSIGN CTANE     = CTANE + ACD_AMT
                  CTANED    = CTANED + ACD_CURR_AMT
                  CTACA     = CTACA + ACD_DR_AMT
                  CTAAB     = CTAAB + (ACD_CR_AMT * -1)
                  CATO      = CATO + ACD_DR_AMT
                  ABTO      = ABTO + (ACD_CR_AMT * -1)
                  NETO      = NETO + ACD_AMT
                  NETOD     = NETOD + ACD_CURR_AMT
                  SB_CTACA  = SB_CTACA + ACD_DR_AMT
                  SB_CTAAB  = SB_CTAAB + (ACD_CR_AMT * -1)
                  SB_CTANE  = SB_CTANE + ACD_AMT
                  SB_CTANED = SB_CTANED + ACD_CURR_AMT.
    END. /**** ACD_DET ***********/

   /**** LMMA AGOSTO 2000 CAMBIA VERSION 9 *******************************   
      /* CALCULO DE SALDO ANTERIOR DE LAS CUENTAS */
       for each acd_det where
                        acd_acc  = asc_acc and
                        acd_sub  = asc_sub and
                        acd_cc   = asc_cc  and
                        acd_year <= an      and        /**** EMS 01/96 ****/
               /*       acd_year = an - 1 and*/
                       (acd_entity = ent 
/** LMMA-1 **/          OR ent = v_inclu):

/* SOLAMENTE SELECCIONA REGISTROS DE PERIODO DOCE, A¥O ANTERIOR */
/*              if acd_year = an - 1 and
                   acd_per >= 1 and acd_per <= 11 then next.

                if (acd_year = an - 1 and acd_per = 12)  or
                   (acd_year = an and acd_per < per) then   **** EMS 01/96 ***/

/* EMS 01/96 */ if (acd_year < an and (tipo<>"E" and tipo<>"I")) or
                   (acd_year = an and acd_per < per) then
                    assign sdoict  = sdoict  + acd_amt
                           sdoictd = sdoictd + acd_curr_amt.

       end. /* For each acd_det */
      /* MOVIMIENTO DEL MES */
      for each gltr_hist where gltr_acc = asc_acc   and
                              gltr_sub  = asc_sub   and
                              gltr_ctr  = asc_cc    and
                           gltr_eff_dt >= fechai    and
                           gltr_eff_dt <= fechaf    and
                        (gltr_entity  = ent 
  /** LMMA-1 **/          OR ent = v_inlcu)
                break by gltr_acc by gltr_sub by gltr_ctr:

       if gltr_amt < 0 then 
          assign
             ctaab  = ctaab  + gltr_amt
             ctaabd = ctaabd + gltr_curramt.
       else
          assign
             ctaca  = ctaca  + gltr_amt
             ctacad = ctacad + gltr_curramt.
      end. /*FOR EACH GLTR_HIST*/
   ctane   = ctaca   - ctaab.
   ctaned  = ctacad  - ctaabd.
   sdofct  = sdoict  + ctane.
   sdofctd = sdoictd + ctaned.
   ********************************************************************/
   
   if sdoict <> 0 or ctaca <> 0 or ctaab <> 0 then do:
 
     if swsub = 0 then do:
        disp space(5) {gl0078.i}  /*"     Subcuenta :" */ 
           asc_sub format "x(8)" nomsub format "x(24)"
           with frame x2 width 256 no-labels no-box.  
       swsub = 1.
     end.       
    
   disp
   asc_cc no-label format "x(4)"            
   nomcc no-label format "x(20)" 
   sdoict       format "->>>>,>>>,>>>.99" /* column-label "Saldo Inicial" */
   ctaca        format "->>>>,>>>,>>>.99" /* column-label "C a r g o s"   */
   ctaab        format "->>>>,>>>,>>>.99" /* column-label "A b o n o s"   */
   ctane        format "->>>>,>>>,>>>.99" /* column-label "Saldo del Mes" */
   sdofct       format "->>>>,>>>,>>>.99" /* column-label "Saldo Acumulado" */
   with frame x.
   
   end.  /* if sdoict */
   
   if (sdoictd <> 0 or ctaned <> 0) and 
       moneda <> wmndbase /*"pes"*/               /** EMS-2 **/ 
   then do:
     
     disp
     sdoictd        format "->>>>,>>>,>>>.99" /* column-label "<---------------!
  Saldo Inicial" */
     ctaned         format "->>>>,>>>,>>>.99" /* column-label "MON. EXTRANJERA!
  Movimiento Neto" */
     sdofctd        format "->>>>,>>>,>>>.99" /* column-label "--------------->!
  Saldo Final" */
     moneda                                   /* column-label "Mon" */
     with frame x.
 
   /******** LMMA AGOSTO 2000 CAMBIA A VERSION 9 ********************
   sb_ctaned = sb_ctaned + ctaned.
   sb_sdoictd= sb_sdoictd + sdoictd.
   sb_sdofctd= sb_sdofctd + sdofctd.
   *********************************************************/
    
   end.

   /********LMMA AGOSTO 2000 CAMBIA A VERSION 9 ******************** 
   sb_sdoict = sb_sdoict + sdoict.
   sb_ctaca  = sb_ctaca  + ctaca.
   sb_ctaab  = sb_ctaab  + ctaab.
   sb_ctane  = sb_ctane + ctane.
   sb_sdofct = sb_sdofct + sdofct.
   
   sdoito = sdoito + sdoict.
   cato   = cato   + ctaca.
   abto   = abto   + ctaab.
   catod  = catod  + ctacad.
   abtod  = abtod  + ctaabd.
   **********************************************************/
   
   assign sdoict = 0   ctaca  = 0 ctaab  = 0  ctane = 0  sdofct = 0 ctaab1 = 0
         sdoictd = 0   ctacad = 0 ctaabd = 0 ctaned = 0 sdofctd = 0.

   if last-of(asc_sub) and swsub=1 then do:
      cuenta = asc_acc + "-" + asc_sub.  /* HSM */
	  hDoc:create-node(hCtas,"BCE:Ctas","ELEMENT").   /* HSM */
      hCtas:set-attribute("NumCta",cuenta). /* HSM */
      hCtas:set-attribute("SaldoIni",trim(string(sb_sdoict,"->>>>>>>>>9.99"))). /* HSM */
      hCtas:set-attribute("Debe",trim(string(sb_ctaca,"->>>>>>>>>9.99"))). /* HSM */
      hCtas:set-attribute("Haber",trim(string(sb_ctaab,"->>>>>>>>>9.99"))). /* HSM */
      hCtas:set-attribute("SaldoFin",trim(string(sb_sdofct,"->>>>>>>>>9.99"))). /* HSM */
      hRoot:append-child(hCtas). /* HSM */
      totalctas = totalctas + 1.  /* HSM */
  
  down(1).
      underline sdoict ctaca ctaab ctane sdofct sdoictd ctaned sdofctd
      with frame x.
  
      disp "TOTAL" @ asc_cc
           sb_sdoict @ sdoict
           sb_ctaca  @ ctaca
           sb_ctaab  @ ctaab
           sb_ctane  @ ctane
           sb_sdofct @ sdofct
           sb_ctaned @ ctaned
           sb_sdoictd @ sdoictd
           sb_sdofctd @ sdofctd with frame x.
      
      if moneda <> wmndbase   /*"pes"*/ then          /** EMS-2 **/
        disp 
          {gr0118.i} /*" T.C."*/ at  133  /*145*/
          (sb_sdofct / sb_sdofctd) format "->>9.999" 
           with frame xy width 170 no-box
           no-labels.
 
      assign sb_sdoict =  0  sb_ctaca  = 0 sb_ctaab = 0  sb_ctane = 0  
             sb_sdofct =  0  sb_sdoictd = 0  sb_ctaned = 0 sb_sdofctd = 0.
   end.

   if last(asc_acc) then do /*with frame x */:
     down(1).
     underline sdoict ctaca ctaab ctane sdofct sdoictd ctaned sdofctd
     with frame x.
     /********LMMA AGOSTO 2000 CAMBIA A VERSION 9 ********************
     neto    = cato    - abto.
     netod   = catod   + abtod.
     sdofto  = sdoito  + neto.
     sdoftod = sdoitod + netod.
     *****************************************************************/
     put control chr(27) + chr(71).
     disp
     "TOTAL"         @ asc_cc
     sdoito          @ sdoict
     cato            @ ctaca
     abto            @ ctaab
     neto            @ ctane
     sdofto          @ sdofct
     sdoitod         @ sdoictd
     netod           @ ctaned
     sdoftod         @ sdofctd
     with frame x.
     assign sdoito = 0    cato = 0 abto = 0 neto = 0 sdofto = 0 sdoitod = 0
             netod = 0 sdoftod = 0.
   end.  /* if last(asc_acc) */

  end. /* for each asc_mstr */
  
  hRoot:set-attribute("TotalCtas",string(totalctas)). /* HSM */
  hRoot:set-attribute("Mes",string(per,"99")). /* HSM */
  hRoot:set-attribute("Ano",string(an)). /* HSM */
  hDoc:append-child(hRoot). /* HSM */
  hDoc:save("file",xmlFilename). /* HSM */
  
  os-command zip value(zipFilename) value(xmlFilename).
  os-command "Envio de zip y xml" | mail -a value(zipFilename) -a value(xmlFilename) jramos@hendrickson-intl.com.
  
  
   end. /* IF AVAILABLE GLC_CAL*/

   else do:
    bell.
    disp {gr0139.i}
 /*   "COMBINACION DE PERIODO Y AÑO NO EXISTEN EN EL CALENDARIO CONTABLE" */
    with frame z color message row 19 no-attr-space.
   end. /*ELSE DO*/
  {finfin.i}
end. /*REPEAT*/
