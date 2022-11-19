       IDENTIFICATION DIVISION.                                         
        PROGRAM-ID PGMVAC15.                                            
                                                                        
      ***********************************************                   
      *                                             *                   
      *       CHECK-POINT 17 BATCH IMPRESION        *                   
      *           TP OPCIONAL - 30-10-2022          *                   
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
                                                                        
             SELECT CLIENTE ASSIGN DDCLIEN                              
                    ORGANIZATION IS INDEXED                             
                    ACCESS MODE IS RANDOM                               
                    RECORD KEY IS KEY-CLI                               
                    FILE STATUS IS FS-CLI.                              
                                                                        
             SELECT SALIDA ASSIGN DDSALID                               
                    FILE STATUS IS FS-SAL.                              
                                                                        
             SELECT LISTADO   ASSIGN DDLISTA                            
                    FILE STATUS IS FS-LIS.                              
                                                                        
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD ENTRADA                                                       
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
                                                                        
       01 REG-ENTRADA    PIC X(82).                                     
                                                                        
       FD CLIENTE.                                                      
                                                                        
       01 REG-CLIENTE.                                                  
          03 KEY-CLI.                                                   
             05 K-TIP    PIC X(2).                                      
             05 K-NRO    PIC X(11).                                     
          03 FILLER      PIC X(147).                                    
                                                                        
       FD SALIDA                                                        
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
                                                                        
       01 REG-SALIDA     PIC  X(84).                                    
                                                                        
       FD LISTADO                                                       
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
                                                                        
       01 REG-LISTADO    PIC  X(132).                                   
                                                                        
                                                                        
       WORKING-STORAGE SECTION.                                         
      **************************************                            
                                                                        
       77  FS-ENT          PIC XX     VALUE SPACES.                     
       77  FS-CLI          PIC XX     VALUE SPACES.                     
       77  FS-SAL          PIC XX     VALUE SPACES.                     
       77  FS-LIS          PIC XX     VALUE SPACES.                     
                                                                        
       01  WS-STATUS-FIN   PIC X.                                       
           88  WS-FIN-LECTURA         VALUE 'Y'.                        
           88  WS-NO-FIN-LECTURA      VALUE 'N'.                        
                                                                        
       01  WS-VALIDADOR    PIC X.                                       
           88  WS-ERROR               VALUE 'Y'.                        
           88  WS-NO-ERROR            VALUE 'N'.                        
                                                                        
       01  WS-FECHA-ACTUAL.                                             
           03 WS-ANIO               PIC 9(4)     VALUE ZEROS.           
           03 FILLER                PIC X(1)     VALUE '/'.             
           03 WS-MES                PIC 9(2)     VALUE ZEROS.           
           03 FILLER                PIC X(1)     VALUE '/'.             
           03 WS-DIA                PIC 9(2)     VALUE ZEROS.           
                                                                        
      *RUTINA DE VALIDACION DE FECHA ******************************      
       01  WS-PGMACAR               PIC X(8)     VALUE 'PGMACAR'.       
                                                                        
           COPY CPNOVED.                                                
           COPY CPPERSON.                                               
           COPY CPNOVEVA REPLACING                                      
                WC-TBCUEN                  BY  WK-TBCUEN                
                WC-CUE-SECUENCIA           BY  WK-CUE-SECUENCIA         
                WC-CUE-TIPO-NOVEDAD        BY  WK-CUE-TIPO-NOVEDAD      
                WC-CUE-TIPO-CUENTA         BY  WK-CUE-TIPO-CUENTA       
                WC-CUE-NRO-CUENTA          BY  WK-CUE-NRO-CUENTA        
                WC-CUE-MONEDA              BY  WK-CUE-MONEDA            
                WC-CUE-CBU                 BY  WK-CUE-CBU               
                WC-CUE-TIP-DOC             BY  WK-CUE-TIP-DOC           
                WC-CUE-NRO-DOC             BY  WK-CUE-NRO-DOC           
                WC-CUE-SALDO-ACTUAL        BY  WK-CUE-SALDO-ACTUAL      
                WC-CUE-FECHA-ACTUAL        BY  WK-CUE-FECHA-ACTUAL      
                WC-CUE-FECHA-ULTIMO-CIERRE BY  WK-CUE-FECHA-ULTIMO.     
                                                                        
       77  WS-SECUENCIA    PIC 99     VALUE ZEROS.                      
       77  WS-TOT-LEI      PIC 999    VALUE ZEROS.                      
       77  WS-TOT-ERR      PIC 999    VALUE ZEROS.                      
       77  WS-TOT-ENC      PIC 999    VALUE ZEROS.                      
       77  WS-TOT-GRAB     PIC 999    VALUE ZEROS.                      
       77  WS-PRINT        PIC ZZ9    VALUE ZEROS.                      
                                                                        
       01  WS-REG-REGISTRO.                                             
           03  FILLER         PIC X(7)     VALUE SPACES.                
           03  WS-TIPO-NOV    PIC X(2)     VALUE SPACES.                
           03  FILLER         PIC X(12)    VALUE SPACES.                
           03  WS-TIPO-CUE    PIC X(2)     VALUE SPACES.                
           03  FILLER         PIC X(9)     VALUE SPACES.                
           03  WS-MONEDA      PIC 9(2)     VALUE ZEROS.                 
           03  FILLER         PIC X(9)     VALUE SPACES.                
           03  WS-TIPO-DOC    PIC X(2)     VALUE SPACES.                
           03  FILLER         PIC X(6)     VALUE SPACES.                
           03  WS-NRO-DOC     PIC 9(11)    VALUE ZEROS.                 
           03  FILLER         PIC X(3)     VALUE SPACES.                
           03  WS-NRO-CUE     PIC 9(15)    VALUE ZEROS.                 
           03  WS-SALDO       PIC -ZZZ.ZZZ.ZZZ.ZZ9,99    VALUE ZEROS.   
           03  FILLER         PIC X        VALUE SPACES.                
           03  WS-FEC-ACT     PIC X(10)    VALUE SPACES.                
           03  FILLER         PIC X(4)     VALUE SPACES.                
           03  WS-FEC-ULT     PIC X(10)    VALUE SPACES.                
           03  FILLER         PIC X(2)     VALUE SPACES.                
                                                                        
       01  WS-REG-TITULO.                                               
           03  FILLER         PIC X        VALUE '*'.                   
           03  FILLER         PIC X(45)    VALUE SPACES.                
           03  FILLER         PIC X(31)                                 
                               VALUE 'VALIDACION DE CLIENTE A FECHA: '. 
           03  WS-DD          PIC Z9       VALUE ZEROS.                 
           03  FILLER         PIC X        VALUE '/'.                   
           03  WS-MM          PIC Z9       VALUE ZEROS.                 
           03  FILLER         PIC X        VALUE '/'.                   
           03  FILLER         PIC 99       VALUE 20.                    
           03  WS-AA          PIC 99       VALUE ZEROS.                 
           03  FILLER         PIC X(33)    VALUE SPACES.                
           03  FILLER         PIC X(08)    VALUE 'PAGINA: '.            
           03  WS-PAGINA      PIC Z9       VALUE ZEROS.                 
           03  FILLER         PIC X        VALUE SPACES.                
           03  FILLER         PIC X        VALUE '*'.                   
                                                                        
       01  WS-REG-SUBTITULO.                                            
           03  FILLER         PIC X(14)    VALUE '* TIPO NOVEDAD'.      
           03  FILLER         PIC X        VALUE SPACES.                
           03  FILLER         PIC X(13)    VALUE ' TIPO CUENTA '.       
           03  FILLER         PIC X        VALUE SPACES.                
           03  FILLER         PIC X(7)     VALUE ' MONEDA'.             
           03  FILLER         PIC X        VALUE SPACES.                
           03  FILLER         PIC X(14)    VALUE 'TIPO DOCUMENTO'.      
           03  FILLER         PIC X        VALUE SPACES.                
           03  FILLER         PIC X(13)    VALUE 'NRO DOCUMENTO'.       
           03  FILLER         PIC X        VALUE SPACES.                
           03  FILLER         PIC X(14)    VALUE '  NRO CUENTA  '.      
           03  FILLER         PIC X        VALUE SPACES.                
           03  FILLER         PIC X(18)    VALUE '       SALDO      '.  
           03  FILLER         PIC X        VALUE SPACES.                
           03  FILLER         PIC X(12)    VALUE 'FECHA ACTUAL'.        
           03  FILLER         PIC X        VALUE SPACES.                
           03  FILLER         PIC X(19)    VALUE 'FECHA ULT CIERRE  *'. 
                                                                        
       01  WS-REG-ERROR.                                                
           03  FILLER         PIC X(3)     VALUE SPACES.                
           03  FILLER         PIC X(26)                                 
                               VALUE 'ERROR ENCONTRADO EN CAMPO '.      
           03  WS-ERROR-TIPO  PIC X(30)    VALUE ZEROS.                 
           03  FILLER         PIC X(1)     VALUE SPACES.                
                                                                        
       01  WS-SEPARADOR       PIC X(132)   VALUE ALL '*'.               
       01  WS-SEPARADOR-REG   PIC X(132)   VALUE ALL '='.               
                                                                        
       01  WS-REG-VALIDO.                                               
           03  FILLER         PIC X(3)     VALUE SPACES.                
           03  FILLER         PIC X(17)   VALUE 'REGISTRO VALIDO. '.    
           03  WS-VAL-CAMPO   PIC X(25)    VALUE SPACES.                
                                                                        
       01  WS-FECHA.                                                    
           03  WS-FECHA-AA    PIC 99       VALUE ZEROS.                 
           03  WS-FECHA-MM    PIC 99       VALUE ZEROS.                 
           03  WS-FECHA-DD    PIC 99       VALUE ZEROS.                 
                                                                        
       77  WS-CUENTA-LINEA    PIC 9(02)    VALUE ZEROS.                 
       77  WS-CUENTA-PAGINA   PIC 9(02)    VALUE 01.                    
       77  WS-CUENTA-ERROR    PIC 9(02)    VALUE ZEROS.                 
                                                                        
                                                                        
       LINKAGE SECTION.                                                 
                                                                        
       01  LK-AREA.                                                     
           03  LK-FECHA.                                                
               05  LK-ANIO    PIC 9(4).                                 
               05  LK-MES     PIC 99.                                   
               05  LK-DIA     PIC 99.                                   
           03  FILLER         PIC X(22).                                
                                                                        
                                                                        
       PROCEDURE DIVISION USING LK-AREA.                                
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
                                                                        
           OPEN INPUT CLIENTE.                                          
           IF FS-CLI IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN CLIENTE INICIO = ' FS-CLI        
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
           OPEN OUTPUT SALIDA.                                          
           IF FS-SAL IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN SALIDA INICIO = ' FS-SAL         
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
                                                                        
           PERFORM 3000-VALIDACION THRU 3000-F-VALIDACION.              
           IF WS-ERROR THEN                                             
              ADD 1 TO WS-TOT-ERR                                       
           ELSE                                                         
              PERFORM 3100-VALIDACION-VSAM THRU 3100-F-VALIDACION-VSAM  
              PERFORM 5200-IMPRIMIR-VALIDO THRU 5200-F-IMPRIMIR-VALIDO  
           END-IF.                                                      
                                                                        
           PERFORM 2100-LECTURA THRU 2100-F-LECTURA.                    
                                                                        
                                                                        
       2000-F-PROCESO. EXIT.                                            
                                                                        
      ***** LECTURA ARCHIVO ENTRADA ********                            
       2100-LECTURA.                                                    
      **************************************                            
                                                                        
           READ ENTRADA INTO WC-TBCUEN.                                 
           EVALUATE FS-ENT                                              
             WHEN '00'                                                  
              ADD 1 TO WS-TOT-LEI                                       
              PERFORM 5000-IMPRIMIR-REGISTRO                            
                                    THRU 5000-F-IMPRIMIR-REGISTRO       
             WHEN '10'                                                  
              SET WS-FIN-LECTURA TO TRUE                                
                                                                        
             WHEN OTHER                                                 
              DISPLAY '*ERROR EN LECTURA ENTRADA : ' FS-ENT             
              SET WS-FIN-LECTURA TO TRUE                                
                                                                        
           END-EVALUATE.                                                
                                                                        
                                                                        
       2100-F-LECTURA. EXIT.                                            
                                                                        
                                                                        
      ***** VALIDACION DE CAMPOS ***********                            
       3000-VALIDACION.                                                 
      **************************************                            
                                                                        
           SET WS-NO-ERROR TO TRUE.                                     
                                                                        
           IF WC-CUE-TIPO-NOVEDAD IS EQUAL TO 'AL' OR 'BA' OR 'MO'      
              CONTINUE                                                  
           ELSE                                                         
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'NOVEDAD' TO WS-ERROR-TIPO                           
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WC-CUE-TIPO-CUENTA IS EQUAL TO 'CC' OR 'CA'               
              CONTINUE                                                  
           ELSE                                                         
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'TIPO DE CUENTA' TO WS-ERROR-TIPO                    
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WC-CUE-NRO-CUENTA IS NOT NUMERIC                          
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'NRO DE CUENTA' TO WS-ERROR-TIPO                     
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WC-CUE-MONEDA IS EQUAL TO 02 OR 80                        
              CONTINUE                                                  
           ELSE                                                         
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'MONEDA' TO WS-ERROR-TIPO                            
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WC-CUE-CBU IS NOT NUMERIC                                 
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'CBU' TO WS-ERROR-TIPO                               
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WC-CUE-TIP-DOC IS EQUAL TO                                
                             'DU' OR 'PA' OR 'PE'                       
              CONTINUE                                                  
           ELSE                                                         
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'TIPO DE DOCUMENTO' TO WS-ERROR-TIPO                 
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WC-CUE-NRO-DOC IS NOT NUMERIC OR                          
              WC-CUE-NRO-DOC < 1                                        
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'NRO DE DOCUMENTO' TO WS-ERROR-TIPO                  
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WC-CUE-SALDO-ACTUAL IS NOT NUMERIC                        
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'SALDO' TO WS-ERROR-TIPO                             
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           MOVE WC-CUE-FECHA-ACTUAL TO WS-FECHA-ACTUAL.                 
                                                                        
           INITIALIZE LK-FECHA.                                         
           MOVE WS-ANIO TO LK-ANIO                                      
           MOVE WS-MES  TO LK-MES                                       
           MOVE WS-DIA  TO LK-DIA                                       
                                                                        
           CALL WS-PGMACAR USING LK-FECHA.                              
                                                                        
           IF RETURN-CODE IS EQUAL TO 05                                
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'FECHA ACTUAL' TO WS-ERROR-TIPO                      
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           MOVE WC-CUE-FECHA-ULTIMO-CIERRE TO WS-FECHA-ACTUAL.          
                                                                        
           INITIALIZE LK-FECHA.                                         
           MOVE WS-ANIO TO LK-ANIO                                      
           MOVE WS-MES  TO LK-MES                                       
           MOVE WS-DIA  TO LK-DIA                                       
                                                                        
           CALL WS-PGMACAR USING LK-FECHA.                              
                                                                        
           IF RETURN-CODE IS EQUAL TO 05                                
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'FECHA ULTIMO CIERRE' TO WS-ERROR-TIPO               
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
                                                                        
       3000-F-VALIDACION. EXIT.                                         
                                                                        
                                                                        
       3100-VALIDACION-VSAM.                                            
                                                                        
            INITIALIZE WS-VAL-CAMPO                                     
            MOVE WC-CUE-TIP-DOC TO K-TIP.                               
            MOVE WC-CUE-NRO-DOC TO K-NRO.                               
                                                                        
            READ CLIENTE.                                               
                                                                        
            EVALUATE FS-CLI                                             
              WHEN '00'                                                 
                ADD 1 TO WS-SECUENCIA                                   
                ADD 1 TO WS-TOT-ENC                                     
                MOVE 'CLIENTE ENCONTRADO' TO WS-VAL-CAMPO               
                PERFORM 4000-GRABAR THRU 4000-F-GRABAR                  
              WHEN '23'                                                 
                MOVE 'CLIENTE NO ENCONTRADO' TO WS-VAL-CAMPO            
              WHEN OTHER                                                
                DISPLAY 'ERROR EN LECTURA CLIENTE = ' FS-CLI            
                SET WS-FIN-LECTURA TO TRUE                              
                                                                        
            END-EVALUATE.                                               
                                                                        
       3100-F-VALIDACION-VSAM. EXIT.                                    
                                                                        
                                                                        
      ***** GRABACION ARCHIVO SALIDA *******                            
       4000-GRABAR.                                                     
      **************************************                            
                                                                        
           INITIALIZE WK-TBCUEN                                         
                                                                        
           MOVE WS-SECUENCIA               TO WK-CUE-SECUENCIA.         
           MOVE WC-CUE-TIPO-NOVEDAD        TO WK-CUE-TIPO-NOVEDAD.      
           MOVE WC-CUE-TIPO-CUENTA         TO WK-CUE-TIPO-CUENTA.       
           MOVE WC-CUE-NRO-CUENTA          TO WK-CUE-NRO-CUENTA.        
           MOVE WC-CUE-MONEDA              TO WK-CUE-MONEDA.            
           MOVE WC-CUE-CBU                 TO WK-CUE-CBU.               
           MOVE WC-CUE-TIP-DOC             TO WK-CUE-TIP-DOC.           
           MOVE WC-CUE-NRO-DOC             TO WK-CUE-NRO-DOC.           
           MOVE WC-CUE-SALDO-ACTUAL        TO WK-CUE-SALDO-ACTUAL.      
           MOVE WC-CUE-FECHA-ACTUAL        TO WK-CUE-FECHA-ACTUAL.      
           MOVE WC-CUE-FECHA-ULTIMO-CIERRE TO WK-CUE-FECHA-ULTIMO.      
                                                                        
           WRITE REG-SALIDA   FROM WK-TBCUEN                            
                                                                        
           IF FS-SAL EQUAL ZEROS                                        
                ADD 1 TO WS-TOT-GRAB                                    
           ELSE                                                         
                DISPLAY '* ERROR EN WRITE SALIDA  = '                   
                                            FS-SAL                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
           END-IF.                                                      
                                                                        
                                                                        
       4000-F-GRABAR. EXIT.                                             
                                                                        
       5000-IMPRIMIR-REGISTRO.                                          
                                                                        
           IF WS-CUENTA-LINEA GREATER 59                                
               PERFORM 5500-IMPRIMIR-TITULOS THRU                       
                     5500-F-IMPRIMIR-TITULOS                            
           END-IF.                                                      
                                                                        
           INITIALIZE WS-REG-REGISTRO.                                  
           MOVE WC-CUE-TIPO-NOVEDAD         TO WS-TIPO-NOV              
           MOVE WC-CUE-TIPO-CUENTA          TO WS-TIPO-CUE              
           MOVE WC-CUE-NRO-CUENTA           TO WS-NRO-CUE               
           MOVE WC-CUE-MONEDA               TO WS-MONEDA                
           MOVE WC-CUE-TIP-DOC              TO WS-TIPO-DOC              
           MOVE WC-CUE-NRO-DOC              TO WS-NRO-DOC               
           MOVE WC-CUE-SALDO-ACTUAL         TO WS-SALDO                 
           MOVE WC-CUE-FECHA-ACTUAL         TO WS-FEC-ACT               
           MOVE WC-CUE-FECHA-ULTIMO-CIERRE  TO WS-FEC-ULT               
                                                                        
           WRITE REG-LISTADO  FROM WS-REG-REGISTRO AFTER 1.             
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
                                                                        
       5200-IMPRIMIR-VALIDO.                                            
                                                                        
           IF WS-CUENTA-LINEA GREATER 59                                
               PERFORM 5500-IMPRIMIR-TITULOS THRU                       
                     5500-F-IMPRIMIR-TITULOS                            
           END-IF.                                                      
                                                                        
           WRITE REG-LISTADO  FROM WS-REG-VALIDO AFTER 1.               
              IF FS-LIS IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE LISTADO = ' FS-LIS            
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
             END-IF.                                                    
                                                                        
             ADD 1 TO WS-CUENTA-LINEA.                                  
                                                                        
       5200-F-IMPRIMIR-VALIDO. EXIT.                                    
                                                                        
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
           MOVE 5 TO WS-CUENTA-LINEA.                                   
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
                                                                        
           WRITE REG-LISTADO FROM WS-REG-SUBTITULO AFTER 1.             
                                                                        
              IF FS-LIS IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE LISTADO = '                   
                                            FS-LIS                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
             END-IF.                                                    
                                                                        
           WRITE REG-LISTADO FROM WS-SEPARADOR-REG AFTER 1.             
                                                                        
              IF FS-LIS IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE LISTADO = '                   
                                            FS-LIS                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
             END-IF.                                                    
                                                                        
       5500-F-IMPRIMIR-TITULOS. EXIT.                                   
                                                                        
       9999-I-FINAL.                                                    
                                                                        
           MOVE WS-TOT-LEI TO WS-PRINT                                  
           DISPLAY "NOVEDADES LEIDAS = " WS-PRINT                       
           MOVE WS-TOT-ENC TO WS-PRINT                                  
           DISPLAY "NOVEDADES ENCONTRADAS = " WS-PRINT                  
           MOVE WS-TOT-ERR TO WS-PRINT                                  
           DISPLAY "NOVEDADES CON ERROR = " WS-PRINT                    
           MOVE  WS-TOT-GRAB TO WS-PRINT                                
           DISPLAY "REGISTROS GRABADOS = " WS-PRINT                     
                                                                        
           CLOSE ENTRADA                                                
              IF FS-ENT IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN CLOSE ENTRADA = '                   
                                            FS-ENT                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
              END-IF.                                                   
                                                                        
           CLOSE CLIENTE                                                
              IF FS-CLI IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN CLOSE ENTRADA = '                   
                                            FS-CLI                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
              END-IF.                                                   
                                                                        
           CLOSE SALIDA                                                 
              IF FS-SAL IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN CLOSE ENTRADA = '                   
                                            FS-SAL                      
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
                                                                        
                                                                        
       9999-F-FINAL.  EXIT.                                             
                                                                        
      *                                                                 
