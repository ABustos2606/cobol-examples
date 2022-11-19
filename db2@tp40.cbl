       IDENTIFICATION DIVISION.                                         
        PROGRAM-ID PGMDB215.                                            
                                                                        
      ***********************************************                   
      *                                             *                   
      *              CHECK-POINT 32 BATCH           *                   
      *               TP 40 - 14-11-2022            *                   
      *                                             *                   
      ***********************************************                   
                                                                        
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
             SELECT ENTRADA ASSIGN DDENTRA                              
                    FILE STATUS IS FS-ENT.                              
                                                                        
             SELECT LISTADO   ASSIGN DDLISTA                            
                    FILE STATUS IS FS-LIS.                              
                                                                        
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD ENTRADA                                                       
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
                                                                        
       01 REG-ENTRADA    PIC X(80).                                     
                                                                        
       FD LISTADO                                                       
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
                                                                        
       01 REG-LISTADO    PIC  X(132).                                   
                                                                        
                                                                        
       WORKING-STORAGE SECTION.                                         
      **************************************                            
                                                                        
       77  FS-ENT          PIC XX     VALUE SPACES.                     
       77  FS-LIS          PIC XX     VALUE SPACES.                     
       77  WS-SQLCODE    PIC +++999 USAGE DISPLAY VALUE ZEROS.          
                                                                        
       01  WS-STATUS-FIN   PIC X.                                       
           88  WS-FIN-LECTURA         VALUE 'Y'.                        
           88  WS-NO-FIN-LECTURA      VALUE 'N'.                        
                                                                        
       01  WS-VALIDADOR    PIC X.                                       
           88  WS-ERROR               VALUE 'Y'.                        
           88  WS-NO-ERROR            VALUE 'N'.                        
                                                                        
       01  WS-FECHA-ACTUAL.                                             
           03 WS-ANIO               PIC 9(4)     VALUE ZEROS.           
           03 FILLER                PIC X(1)     VALUE '-'.             
           03 WS-MES                PIC 9(2)     VALUE ZEROS.           
           03 FILLER                PIC X(1)     VALUE '-'.             
           03 WS-DIA                PIC 9(2)     VALUE ZEROS.           
                                                                        
       01 WS-STATUS-FECHA           PIC X.                              
          88 WS-FECHA-VALIDA                     VALUE 'Y'.             
          88 WS-FECHA-NOT-VALIDA                 VALUE 'N'.             
                                                                        
       77  WS-RESULTADO             PIC 9(04)    VALUE ZEROS.           
       77  WS-RESTO-4               PIC 9(04)V99 VALUE ZEROS.           
       77  WS-RESTO-100             PIC 9(02)V99 VALUE ZEROS.           
       77  WS-RESTO-400             PIC 9(02)V99 VALUE ZEROS.           
                                                                        
           COPY NOVECLIE.                                               
                                                                        
           EXEC SQL                                                     
             INCLUDE SQLCA                                              
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
             INCLUDE TB99CLIE                                           
           END-EXEC.                                                    
                                                                        
       77  WS-TOT-LEI      PIC 99     VALUE ZEROS.                      
       77  WS-TOT-ERR      PIC 99     VALUE ZEROS.                      
       77  WS-TOT-ALTAS    PIC 99     VALUE ZEROS.                      
       77  WS-TOT-MOD      PIC 99     VALUE ZEROS.                      
                                                                        
       77  WS-PRINT        PIC ZZ9    VALUE ZEROS.                      
                                                                        
       01  WS-REG-LISTADO.                                              
           03  FILLER         PIC X(17)    VALUE 'TIPO DE NOVEDAD: '.   
           03  WS-LIS-NOV     PIC XX       VALUE ZEROS.                 
           03  FILLER         PIC X(9)     VALUE SPACES.                
           03  FILLER         PIC X(19)    VALUE 'TIPO DE DOCUMENTO: '. 
           03  WS-LIS-TIP-DOC PIC XX       VALUE SPACES.                
           03  FILLER         PIC X(5)     VALUE SPACES.                
           03  FILLER         PIC X(18)    VALUE 'NRO DE DOCUMENTO: '.  
           03  WS-LIS-NRO-DOC PIC 9(11)    VALUE ZEROS.                 
           03  FILLER         PIC X        VALUE SPACES.                
                                                                        
       01  WS-REG-ERROR.                                                
           03  FILLER         PIC X(3)     VALUE SPACES.                
           03  FILLER         PIC X(26)                                 
                               VALUE 'ERROR ENCONTRADO EN CAMPO '.      
           03  WS-ERROR-TIPO  PIC X(30)    VALUE ZEROS.                 
           03  FILLER         PIC X(2)     VALUE ': '.                  
           03  WS-ERROR-CAMPO PIC X(30)    VALUE SPACES.                
           03  FILLER         PIC X        VALUE SPACES.                
                                                                        
       01  WS-REG-TITULO.                                               
           03  FILLER         PIC X        VALUE '*'.                   
           03  FILLER         PIC X(45)    VALUE SPACES.                
           03  FILLER         PIC X(32)                                 
                               VALUE '   NOVEDADES CON ERROR - FECHA: '.
           03  WS-DD          PIC Z9       VALUE ZEROS.                 
           03  FILLER         PIC X        VALUE '-'.                   
           03  WS-MM          PIC Z9       VALUE ZEROS.                 
           03  FILLER         PIC X        VALUE '-'.                   
           03  FILLER         PIC 99       VALUE 20.                    
           03  WS-AA          PIC 99       VALUE ZEROS.                 
           03  FILLER         PIC X(32)    VALUE SPACES.                
           03  FILLER         PIC X(08)    VALUE 'PAGINA: '.            
           03  WS-PAGINA      PIC Z9       VALUE ZEROS.                 
           03  FILLER         PIC X        VALUE SPACES.                
           03  FILLER         PIC X        VALUE '*'.                   
                                                                        
       01  WS-SEPARADOR       PIC X(132)   VALUE ALL '*'.               
       01  WS-SEPARADOR-REG   PIC X(132)   VALUE ALL '='.               
                                                                        
       01  WS-FECHA.                                                    
           03  WS-FECHA-AA    PIC 99       VALUE ZEROS.                 
           03  WS-FECHA-MM    PIC 99       VALUE ZEROS.                 
           03  WS-FECHA-DD    PIC 99       VALUE ZEROS.                 
                                                                        
       77  WS-CUENTA-LINEA    PIC 9(02)    VALUE ZEROS.                 
       77  WS-CUENTA-PAGINA   PIC 9(02)    VALUE 01.                    
                                                                        
                                                                        
       PROCEDURE DIVISION.                                              
      *****************************************************             
      **************************************                            
      *                                    *                            
      *  CUERPO PRINCIPAL DEL PROGRAMA     *                            
      *                                    *                            
      **************************************                            
       MAIN-PROGRAM-INICIO.                                             
                                                                        
           PERFORM 1000-I-INICIO  THRU 1000-F-INICIO.                   
                                                                        
           PERFORM 2000-I-PROCESO THRU  2000-F-PROCESO                  
                                  UNTIL WS-FIN-LECTURA.                 
                                                                        
           PERFORM 9999-I-FINAL     THRU  9999-I-FINAL.                 
                                                                        
       MAIN-PROGRAM-FINAL. GOBACK.                                      
                                                                        
      **************************************                            
       1000-I-INICIO.                                                   
      **************************************                            
                                                                        
           ACCEPT WS-FECHA FROM DATE.                                   
           MOVE WS-FECHA-AA TO WS-AA.                                   
           MOVE WS-FECHA-MM TO WS-MM.                                   
           MOVE WS-FECHA-DD TO WS-DD.                                   
           MOVE 62 TO WS-CUENTA-LINEA.                                  
                                                                        
           SET WS-NO-FIN-LECTURA TO TRUE.                               
                                                                        
           OPEN INPUT ENTRADA.                                          
           IF FS-ENT IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN ENTRADA INICIO = ' FS-ENT        
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
           OPEN OUTPUT LISTADO.                                         
           IF FS-LIS IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN IMPRESION INICIO = ' FS-LIS      
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
           PERFORM 2100-LECTURA THRU 2100-F-LECTURA.                    
                                                                        
       1000-F-INICIO. EXIT.                                             
                                                                        
      **************************************                            
       2000-I-PROCESO.                                                  
      **************************************                            
                                                                        
           EVALUATE NOV-TIP-NOV                                         
              WHEN 'AL'                                                 
                 PERFORM 3000-VALIDACION-AL THRU 3000-F-VALIDACION-AL   
                 IF WS-ERROR THEN                                       
                    ADD 1 TO WS-TOT-ERR                                 
                 ELSE                                                   
                    PERFORM 4000-INSERT-AL THRU 4000-F-INSERT-AL        
                 END-IF                                                 
              WHEN 'CN'                                                 
                 PERFORM 3010-VALIDACION-CN THRU 3010-F-VALIDACION-CN   
                 IF WS-ERROR THEN                                       
                    ADD 1 TO WS-TOT-ERR                                 
                 ELSE                                                   
                    PERFORM 4010-UPDATE-CN THRU 4010-F-UPDATE-CN        
                 END-IF                                                 
              WHEN 'CL'                                                 
                 PERFORM 3020-VALIDACION-CL THRU 3020-F-VALIDACION-CL   
                 IF WS-ERROR THEN                                       
                    ADD 1 TO WS-TOT-ERR                                 
                 ELSE                                                   
                    PERFORM 4020-UPDATE-CL THRU 4020-F-UPDATE-CL        
                 END-IF                                                 
              WHEN 'CX'                                                 
                 PERFORM 3030-VALIDACION-CX THRU 3030-F-VALIDACION-CX   
                 IF WS-ERROR THEN                                       
                    ADD 1 TO WS-TOT-ERR                                 
                 ELSE                                                   
                    PERFORM 4030-UPDATE-CX THRU 4030-F-UPDATE-CX        
                 END-IF                                                 
              WHEN OTHER                                                
                 ADD 1 TO WS-TOT-ERR                                    
                 PERFORM 5000-IMPRIMIR-REGISTRO                         
                         THRU 5000-F-IMPRIMIR-REGISTRO                  
                 INITIALIZE WS-REG-ERROR                                
                 MOVE 'NOVEDAD' TO WS-ERROR-TIPO                        
                 MOVE NOV-TIP-NOV TO WS-ERROR-CAMPO                     
                 PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR 
           END-EVALUATE.                                                
                                                                        
           PERFORM 2100-LECTURA THRU 2100-F-LECTURA.                    
                                                                        
                                                                        
       2000-F-PROCESO. EXIT.                                            
                                                                        
      ***** LECTURA ARCHIVO NOVEDADES ******                            
       2100-LECTURA.                                                    
      **************************************                            
                                                                        
           READ ENTRADA INTO WS-REG-NOVECLI.                            
           EVALUATE FS-ENT                                              
             WHEN '00'                                                  
              ADD 1 TO WS-TOT-LEI                                       
              SET WS-NO-ERROR TO TRUE                                   
             WHEN '10'                                                  
              SET WS-FIN-LECTURA TO TRUE                                
                                                                        
             WHEN OTHER                                                 
              DISPLAY '*ERROR EN LECTURA ENTRADA : ' FS-ENT             
              SET WS-FIN-LECTURA TO TRUE                                
                                                                        
           END-EVALUATE.                                                
                                                                        
                                                                        
       2100-F-LECTURA. EXIT.                                            
                                                                        
                                                                        
      ***** VALIDACION DE CAMPOS ***********                            
       3000-VALIDACION-AL.                                              
      **************************************                            
                                                                        
           SET WS-NO-ERROR TO TRUE.                                     
                                                                        
           IF NOV-TIP-DOC IS EQUAL TO                                   
                             'DU' OR 'PA' OR 'LE' OR 'LC'               
              CONTINUE                                                  
           ELSE                                                         
              IF WS-NO-ERROR                                            
                 PERFORM 5000-IMPRIMIR-REGISTRO                         
                         THRU 5000-F-IMPRIMIR-REGISTRO                  
              END-IF                                                    
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'TIPO DE DOCUMENTO' TO WS-ERROR-TIPO                 
              MOVE NOV-TIP-DOC TO WS-ERROR-CAMPO                        
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF NOV-NRO-DOC IS NOT NUMERIC OR                             
              NOV-NRO-DOC < 1                                           
              IF WS-NO-ERROR                                            
                 PERFORM 5000-IMPRIMIR-REGISTRO                         
                         THRU 5000-F-IMPRIMIR-REGISTRO                  
              END-IF                                                    
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'NRO DE DOCUMENTO' TO WS-ERROR-TIPO                  
              MOVE NOV-NRO-DOC TO WS-ERROR-CAMPO                        
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF NOV-CLI-NRO IS NOT NUMERIC OR                             
              NOV-CLI-NRO < 1                                           
              IF WS-NO-ERROR                                            
                 PERFORM 5000-IMPRIMIR-REGISTRO                         
                         THRU 5000-F-IMPRIMIR-REGISTRO                  
              END-IF                                                    
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'NUMERO DE CLIENTE' TO WS-ERROR-TIPO                 
              MOVE NOV-CLI-NRO TO WS-ERROR-CAMPO                        
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF NOV-CLI-NOMBRE IS EQUAL TO SPACES                         
              IF WS-NO-ERROR                                            
                 PERFORM 5000-IMPRIMIR-REGISTRO                         
                         THRU 5000-F-IMPRIMIR-REGISTRO                  
              END-IF                                                    
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'NOMBRE' TO WS-ERROR-TIPO                            
              MOVE NOV-CLI-NOMBRE TO WS-ERROR-CAMPO                     
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF NOV-CLI-SEXO IS EQUAL TO 'F' OR 'M' OR 'O'                
              CONTINUE                                                  
           ELSE                                                         
              IF WS-NO-ERROR                                            
                 PERFORM 5000-IMPRIMIR-REGISTRO                         
                         THRU 5000-F-IMPRIMIR-REGISTRO                  
              END-IF                                                    
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'SEXO' TO WS-ERROR-TIPO                              
              MOVE NOV-CLI-SEXO TO WS-ERROR-CAMPO                       
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           MOVE NOV-CLI-FENAC(1:4) TO WS-ANIO.                          
           MOVE NOV-CLI-FENAC(5:2) TO WS-MES.                           
           MOVE NOV-CLI-FENAC(7:2) TO WS-DIA.                           
                                                                        
           PERFORM 3100-VALIDAR-FECHA                                   
                   THRU 3100-F-VALIDAR-FECHA.                           
                                                                        
           IF WS-FECHA-NOT-VALIDA THEN                                  
              IF WS-NO-ERROR                                            
                 PERFORM 5000-IMPRIMIR-REGISTRO                         
                         THRU 5000-F-IMPRIMIR-REGISTRO                  
              END-IF                                                    
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'FECHA DE NACIMIENTO' TO WS-ERROR-TIPO               
              MOVE NOV-CLI-FENAC TO WS-ERROR-CAMPO                      
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
       3000-F-VALIDACION-AL. EXIT.                                      
                                                                        
       3010-VALIDACION-CN.                                              
                                                                        
           IF NOV-CLI-NOMBRE IS EQUAL TO SPACES                         
              SET WS-ERROR TO TRUE                                      
              PERFORM 5000-IMPRIMIR-REGISTRO                            
                      THRU 5000-F-IMPRIMIR-REGISTRO                     
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'NOMBRE' TO WS-ERROR-TIPO                            
              MOVE NOV-CLI-NOMBRE TO WS-ERROR-CAMPO                     
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
       3010-F-VALIDACION-CN. EXIT.                                      
                                                                        
       3020-VALIDACION-CL.                                              
                                                                        
           IF NOV-CLI-NRO IS NOT NUMERIC OR                             
              NOV-CLI-NRO < 1                                           
              SET WS-ERROR TO TRUE                                      
              PERFORM 5000-IMPRIMIR-REGISTRO                            
                      THRU 5000-F-IMPRIMIR-REGISTRO                     
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'NUMERO DE CLIENTE' TO WS-ERROR-TIPO                 
              MOVE NOV-CLI-NRO TO WS-ERROR-CAMPO                        
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
       3020-F-VALIDACION-CL. EXIT.                                      
                                                                        
       3030-VALIDACION-CX.                                              
                                                                        
           IF NOV-CLI-SEXO IS EQUAL TO 'F' OR 'M' OR 'O'                
              CONTINUE                                                  
           ELSE                                                         
              SET WS-ERROR TO TRUE                                      
              PERFORM 5000-IMPRIMIR-REGISTRO                            
                      THRU 5000-F-IMPRIMIR-REGISTRO                     
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'SEXO' TO WS-ERROR-TIPO                              
              MOVE NOV-CLI-SEXO TO WS-ERROR-CAMPO                       
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
       3030-F-VALIDACION-CX. EXIT.                                      
                                                                        
      ***** VALIDACION DE FECHA ************                            
       3100-VALIDAR-FECHA.                                              
      **************************************                            
                                                                        
           SET WS-FECHA-VALIDA TO TRUE.                                 
                                                                        
           IF WS-ANIO IS NOT NUMERIC OR                                 
              WS-MES  IS NOT NUMERIC OR                                 
              WS-DIA  IS NOT NUMERIC                                    
                 SET WS-FECHA-NOT-VALIDA TO TRUE                        
           END-IF.                                                      
                                                                        
           IF WS-FECHA-VALIDA                                           
                 IF WS-ANIO < 1922 OR WS-ANIO > 2003                    
                    SET WS-FECHA-NOT-VALIDA TO TRUE                     
                 END-IF                                                 
                                                                        
                 IF WS-MES < 00 OR WS-MES > 13                          
                    SET WS-FECHA-NOT-VALIDA TO TRUE                     
                 END-IF                                                 
                                                                        
                 IF WS-MES = 02                                         
                  IF WS-DIA > 28                                        
                     IF WS-DIA > 29                                     
                        SET WS-FECHA-NOT-VALIDA TO TRUE                 
                     ELSE                                               
                       DIVIDE WS-ANIO BY 004 GIVING WS-RESULTADO        
                                      REMAINDER WS-RESTO-4              
                       DIVIDE WS-ANIO BY 100 GIVING WS-RESULTADO        
                                      REMAINDER WS-RESTO-100            
                       DIVIDE WS-ANIO BY 400 GIVING WS-RESULTADO        
                                      REMAINDER WS-RESTO-400            
                       IF NOT ((WS-RESTO-4 EQUAL 0 AND                  
                              WS-RESTO-100 NOT EQUAL 0) OR              
                              WS-RESTO-400 EQUAL 0)                     
                                 SET WS-FECHA-NOT-VALIDA TO TRUE        
                     END-IF                                             
                  END-IF                                                
                 END-IF                                                 
                                                                        
                 IF WS-MES IS EQUAL TO (4 OR 6 OR 9 OR 11) AND          
                  WS-DIA > 30                                           
                     SET WS-FECHA-NOT-VALIDA TO TRUE                    
                 END-IF                                                 
                 IF WS-MES IS EQUAL TO                                  
                   (1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12) AND              
                   WS-DIA > 31                                          
                     SET WS-FECHA-NOT-VALIDA TO TRUE                    
                 END-IF                                                 
                                                                        
           END-IF.                                                      
                                                                        
       3100-F-VALIDAR-FECHA. EXIT.                                      
                                                                        
       4000-INSERT-AL.                                                  
                                                                        
           MOVE NOV-TIP-DOC     TO DB-CL-TIPDOC                         
           MOVE NOV-NRO-DOC     TO DB-CL-NRODOC                         
           MOVE NOV-CLI-NRO     TO DB-CL-NROCLI                         
           MOVE NOV-CLI-NOMBRE  TO DB-CL-NOMAPE                         
           MOVE WS-FECHA-ACTUAL TO DB-CL-FECNAC                         
           MOVE NOV-CLI-SEXO    TO DB-CL-SEXO                           
                                                                        
           EXEC SQL                                                     
               INSERT INTO ITPLZRY.TB99CLIE  (TIPDOC,                   
                                              NRODOC,                   
                                              NROCLI,                   
                                              NOMAPE,                   
                                              FECNAC,                   
                                              SEXO)                     
                                                                        
                      VALUES                 (:DB-CL-TIPDOC,            
                                              :DB-CL-NRODOC,            
                                              :DB-CL-NROCLI,            
                                              :DB-CL-NOMAPE,            
                                              :DB-CL-FECNAC,            
                                              :DB-CL-SEXO)              
           END-EXEC.                                                    
                                                                        
           EVALUATE SQLCODE                                             
             WHEN ZEROS                                                 
                ADD 1 TO WS-TOT-ALTAS                                   
             WHEN  -803                                                 
                ADD 1 TO WS-TOT-ERR                                     
                SET WS-ERROR TO TRUE                                    
                INITIALIZE WS-REG-ERROR                                 
                MOVE '***CLIENTE DUPLICADO' TO WS-ERROR-TIPO            
                MOVE NOV-CLI-NRO TO WS-ERROR-CAMPO                      
                PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR  
             WHEN OTHER                                                 
                MOVE SQLCODE TO WS-SQLCODE                              
                DISPLAY 'ERROR INSERT SQL : ' WS-SQLCODE                
                SET WS-FIN-LECTURA TO TRUE                              
           END-EVALUATE.                                                
                                                                        
       4000-F-INSERT-AL. EXIT.                                          
                                                                        
       4010-UPDATE-CN.                                                  
                                                                        
           MOVE NOV-CLI-NOMBRE TO DB-CL-NOMAPE                          
           MOVE NOV-TIP-DOC TO DB-CL-TIPDOC                             
           MOVE NOV-NRO-DOC TO DB-CL-NRODOC                             
                                                                        
           EXEC SQL                                                     
               UPDATE ITPLZRY.TB99CLIE                                  
               SET NOMAPE = :DB-CL-NOMAPE                               
               WHERE TIPDOC = :DB-CL-TIPDOC                             
               AND NRODOC = :DB-CL-NRODOC                               
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT EQUAL ZEROS                                   
              MOVE SQLCODE   TO WS-SQLCODE                              
              DISPLAY '* ERROR UPDATE NOMBRE = ' WS-SQLCODE             
              SET WS-FIN-LECTURA TO TRUE                                
           ELSE                                                         
              ADD 1 TO WS-TOT-MOD                                       
           END-IF.                                                      
                                                                        
       4010-F-UPDATE-CN. EXIT.                                          
                                                                        
       4020-UPDATE-CL.                                                  
                                                                        
           MOVE NOV-CLI-NRO TO DB-CL-NROCLI                             
           MOVE NOV-TIP-DOC TO DB-CL-TIPDOC                             
           MOVE NOV-NRO-DOC TO DB-CL-NRODOC                             
                                                                        
           EXEC SQL                                                     
               UPDATE ITPLZRY.TB99CLIE                                  
               SET NROCLI = :DB-CL-NROCLI                               
               WHERE TIPDOC = :DB-CL-TIPDOC                             
               AND NRODOC = :DB-CL-NRODOC                               
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT EQUAL ZEROS                                   
              MOVE SQLCODE   TO WS-SQLCODE                              
              DISPLAY '* ERROR UPDATE NRO CLIENTE = ' WS-SQLCODE        
              SET WS-FIN-LECTURA TO TRUE                                
           ELSE                                                         
              ADD 1 TO WS-TOT-MOD                                       
           END-IF.                                                      
                                                                        
       4020-F-UPDATE-CL. EXIT.                                          
                                                                        
       4030-UPDATE-CX.                                                  
                                                                        
           MOVE NOV-CLI-SEXO TO DB-CL-SEXO                              
           MOVE NOV-TIP-DOC TO DB-CL-TIPDOC                             
           MOVE NOV-NRO-DOC TO DB-CL-NRODOC                             
                                                                        
           EXEC SQL                                                     
               UPDATE ITPLZRY.TB99CLIE                                  
               SET SEXO = :DB-CL-SEXO                                   
               WHERE TIPDOC = :DB-CL-TIPDOC                             
               AND NRODOC = :DB-CL-NRODOC                               
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT EQUAL ZEROS                                   
              MOVE SQLCODE   TO WS-SQLCODE                              
              DISPLAY '* ERROR UPDATE SEXO = ' WS-SQLCODE               
              SET WS-FIN-LECTURA TO TRUE                                
           ELSE                                                         
              ADD 1 TO WS-TOT-MOD                                       
           END-IF.                                                      
                                                                        
       4030-F-UPDATE-CX. EXIT.                                          
                                                                        
       5000-IMPRIMIR-REGISTRO.                                          
                                                                        
           IF WS-CUENTA-LINEA GREATER 59                                
               PERFORM 5500-IMPRIMIR-TITULOS THRU                       
                     5500-F-IMPRIMIR-TITULOS                            
           END-IF.                                                      
                                                                        
           INITIALIZE WS-REG-LISTADO.                                   
           MOVE NOV-TIP-NOV   TO WS-LIS-NOV.                            
           MOVE NOV-TIP-DOC TO WS-LIS-TIP-DOC.                          
           MOVE NOV-NRO-DOC TO WS-LIS-NRO-DOC.                          
                                                                        
           WRITE REG-LISTADO  FROM WS-REG-LISTADO AFTER 1.              
              IF FS-LIS IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE LISTADO = ' FS-LIS            
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
             END-IF.                                                    
                                                                        
             ADD 1 TO WS-CUENTA-LINEA.                                  
                                                                        
       5000-F-IMPRIMIR-REGISTRO. EXIT.                                  
                                                                        
       5100-IMPRIMIR-ERROR.                                             
                                                                        
           IF WS-CUENTA-LINEA GREATER 59                                
               PERFORM 5500-IMPRIMIR-TITULOS THRU                       
                     5500-F-IMPRIMIR-TITULOS                            
               PERFORM 5000-IMPRIMIR-REGISTRO THRU                      
                     5000-F-IMPRIMIR-REGISTRO                           
           END-IF.                                                      
                                                                        
           WRITE REG-LISTADO  FROM WS-REG-ERROR AFTER 1.                
              IF FS-LIS IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE LISTADO = ' FS-LIS            
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
             END-IF.                                                    
                                                                        
             ADD 1 TO WS-CUENTA-LINEA.                                  
                                                                        
       5100-F-IMPRIMIR-ERROR. EXIT.                                     
                                                                        
       5300-IMPRIMIR-SEPARADOR.                                         
                                                                        
           IF WS-CUENTA-LINEA GREATER 59                                
               PERFORM 5500-IMPRIMIR-TITULOS THRU                       
                     5500-F-IMPRIMIR-TITULOS                            
           ELSE                                                         
                                                                        
              WRITE REG-LISTADO  FROM WS-SEPARADOR-REG AFTER 1          
                 IF FS-LIS IS NOT EQUAL '00'                            
                   DISPLAY '* ERROR EN WRITE LISTADO = ' FS-LIS         
                   MOVE 9999 TO RETURN-CODE                             
                   SET WS-FIN-LECTURA TO TRUE                           
                 END-IF                                                 
                                                                        
              ADD 1 TO WS-CUENTA-LINEA                                  
                                                                        
           END-IF.                                                      
                                                                        
                                                                        
       5300-F-IMPRIMIR-SEPARADOR. EXIT.                                 
                                                                        
       5500-IMPRIMIR-TITULOS.                                           
           MOVE WS-CUENTA-PAGINA      TO WS-PAGINA.                     
           MOVE 3 TO WS-CUENTA-LINEA.                                   
           ADD  1 TO WS-CUENTA-PAGINA.                                  
                                                                        
           WRITE REG-LISTADO FROM WS-SEPARADOR AFTER PAGE.              
                                                                        
              IF FS-LIS IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE LISTADO = '                   
                                            FS-LIS                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
             END-IF.                                                    
                                                                        
           WRITE REG-LISTADO FROM WS-REG-TITULO AFTER 1.                
                                                                        
              IF FS-LIS IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE LISTADO = '                   
                                            FS-LIS                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
             END-IF.                                                    
                                                                        
           WRITE REG-LISTADO FROM WS-SEPARADOR AFTER 1.                 
                                                                        
              IF FS-LIS IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE LISTADO = '                   
                                            FS-LIS                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
             END-IF.                                                    
                                                                        
       5500-F-IMPRIMIR-TITULOS. EXIT.                                   
                                                                        
       9999-I-FINAL.                                                    
                                                                        
           MOVE WS-TOT-LEI TO WS-PRINT                                  
           DISPLAY "TOTAL NOVEDADES LEIDAS       = " WS-PRINT           
           MOVE WS-TOT-ERR TO WS-PRINT                                  
           DISPLAY "TOTAL NOVEDADES CON ERROR    = " WS-PRINT           
           MOVE WS-TOT-ALTAS TO WS-PRINT                                
           DISPLAY "TOTAL ALTA NOVEDADES         = " WS-PRINT           
           MOVE  WS-TOT-MOD TO WS-PRINT                                 
           DISPLAY "TOTAL MODIFICACION NOVEDADES = " WS-PRINT           
                                                                        
           CLOSE ENTRADA                                                
              IF FS-ENT IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN CLOSE ENTRADA = '                   
                                            FS-ENT                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
              END-IF.                                                   
                                                                        
           CLOSE LISTADO                                                
              IF FS-LIS IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN CLOSE LISTADO = '                   
                                            FS-LIS                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
              END-IF.                                                   
                                                                        
      *       EXEC SQL                                                  
      *           ROLLBACK                                              
      *       END-EXEC.                                                 
                                                                        
       9999-F-FINAL.  EXIT.                                             
                                                                        
      *                                                                