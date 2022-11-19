       IDENTIFICATION DIVISION.                                         
      *------------------------*                                        
       PROGRAM-ID. PGMDB115.                                            
      ********************************************************          
      *                                                      *          
      *          TP 43 - CHECK-POINT 34 BATCH CURSOR         *          
      *                     17-11-2022                       *          
      *                                                      *          
      ********************************************************          
       ENVIRONMENT DIVISION.                                            
      *---------------------*                                           
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT SALIDA ASSIGN DDSALID                                 
                  FILE STATUS IS FS-SAL.                                
                                                                        
           SELECT VSAM   ASSIGN DDVSAM                                  
                  ORGANIZATION IS INDEXED                               
                  ACCESS MODE IS SEQUENTIAL                             
                  RECORD KEY IS KEY-VSAM                                
                  FILE STATUS IS FS-VSAM.                               
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  SALIDA                                                       
           BLOCK CONTAINS  0 RECORDS                                    
           RECORDING MODE IS F.                                         
                                                                        
       01  REG-SALIDA         PIC X(100).                               
                                                                        
       FD VSAM.                                                         
                                                                        
       01 REG-VSAM.                                                     
           03 KEY-VSAM.                                                 
              05 KEY-SUCUEN      PIC 9(02).                             
              05 KEY-NROCLI      PIC 9(03).                             
              05 KEY-TIPCUEN     PIC X(02).                             
           03 FILLER             PIC X(83).                             
                                                                        
       WORKING-STORAGE SECTION.                                         
      *------------------------*                                        
                                                                        
       01  FS-SAL             PIC X(02).                                
           88  FS-OK                      VALUE '00'.                   
           88  FS-EOF                     VALUE '10'.                   
           88  FS-NOK                     VALUE '01' THRU '09'          
                                                '11' THRU '99'.         
                                                                        
       01  FS-VSAM            PIC X(02).                                
           88  FS-OK2                     VALUE '00'.                   
           88  FS-EOF2                    VALUE '10'.                   
           88  FS-NOK2                    VALUE '01' THRU '09'          
                                                '11' THRU '99'.         
                                                                        
       01  FS-SQL             PIC X(02).                                
           88  SQL-OK                     VALUE '00'.                   
           88  SQL-EOF                    VALUE '10'.                   
           88  SQL-NOK                    VALUE '01' THRU '09'          
                                                '11' THRU '99'.         
                                                                        
       77  WS-SQLCODE         PIC +++999 USAGE DISPLAY VALUE ZEROS.     
                                                                        
           EXEC SQL                                                     
             INCLUDE SQLCA                                              
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
             INCLUDE TBCURCTA                                           
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
             INCLUDE TBCURCLI                                           
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
             DECLARE CURSOR1 CURSOR FOR                                 
             SELECT TIPCUEN,                                            
                    NROCUEN,                                            
                    SUCUEN,                                             
                    NROCLI,                                             
                    SALDO,                                              
                    FECSAL                                              
             FROM ITPARUT.TBCURCTA                                      
               ORDER BY SUCUEN ASC,                                     
                        NROCLI ASC,                                     
                       TIPCUEN ASC                                      
           END-EXEC.                                                    
                                                                        
      * VARIABLES *                                                     
       01  WS-SUCUEN-ANT      PIC 99.                                   
       01  WS-TIPCUEN-ANT     PIC 99.                                   
                                                                        
      * ACUMULADORES *                                                  
       01  AC-SALCUEN         PIC S9(7)V9(2) USAGE COMP-3  VALUE ZEROS. 
       01  AC-SALSUC          PIC S9(7)V9(2) USAGE COMP-3  VALUE ZEROS. 
       01  AC-SALCLI          PIC S9(7)V9(2) USAGE COMP-3  VALUE ZEROS. 
                                                                        
      * CONTADORES *                                                    
       01  CN-TOT-CLI         PIC 9(03)   VALUE ZEROS.                  
       01  CN-TOT-SUC         PIC 9(03)   VALUE ZEROS.                  
       01  CN-TOT-TIPO        PIC 9(03)   VALUE ZEROS.                  
       77  CN-LINEA           PIC 9(02)   VALUE 63.                     
       77  CN-PAGINA          PIC 9(02)   VALUE ZEROS.                  
                                                                        
       01  VS-GRABACION.                                                
           03  VS-KEY.                                                  
               05  VS-SUCUEN     PIC 99           VALUE ZEROS.          
               05  VS-NROCLI     PIC 9(3)         VALUE ZEROS.          
               05  VS-TIPCUEN    PIC 99           VALUE ZEROS.          
           03  VS-NROCUEN        PIC 9(5)         VALUE ZEROS.          
           03  VS-SALDO          PIC S9(5)V9(2)   VALUE ZEROS.          
           03  VS-FECSAL         PIC X(10)        VALUE ZEROS.          
                                                                        
       01  WS-FECHA.                                                    
           03  WS-FECHA-AA    PIC 99       VALUE ZEROS.                 
           03  WS-FECHA-MM    PIC 99       VALUE ZEROS.                 
           03  WS-FECHA-DD    PIC 99       VALUE ZEROS.                 
                                                                        
      * IMPRESION *                                                     
       01  WS-SEPARADOR       PIC X(132)  VALUE ALL '*'.                
       01  WS-SEPARADOR2      PIC X(132)  VALUE SPACES.                 
                                                                        
       01  WS-TITULO.                                                   
           03  FILLER        PIC X(11)    VALUE 'TIPO CUENTA'.          
           03  FILLER        PIC X(2)     VALUE SPACES.                 
           03  FILLER        PIC X(10)    VALUE 'NRO CUENTA'.           
           03  FILLER        PIC X(2)     VALUE SPACES.                 
           03  FILLER        PIC X(30)    VALUE 'NOMBRE Y APELLIDO'.    
           03  FILLER        PIC X(2)     VALUE SPACES.                 
           03  FILLER        PIC X(11)    VALUE 'NRO CLIENTE'.          
           03  FILLER        PIC X(16)    VALUE SPACES.                 
           03  FILLER        PIC X(5)     VALUE 'SALDO'.                
           03  FILLER        PIC X(57)    VALUE SPACES.                 
                                                                        
       01  WS-REG-CLIENTE.                                              
           03  FILLER        PIC X(4)     VALUE SPACES.                 
           03  WS-TIPCUEN    PIC 99       VALUE ZEROS.                  
           03  FILLER        PIC X(10)    VALUE SPACES.                 
           03  WS-NROCUEN    PIC ZZZZ9    VALUE ZEROS.                  
           03  FILLER        PIC X(4)     VALUE SPACES.                 
           03  WS-NOMYAPEL   PIC X(30)    VALUE SPACES.                 
           03  FILLER        PIC X(6)     VALUE SPACES.                 
           03  WS-NROCLI     PIC ZZ9      VALUE ZEROS.                  
           03  FILLER        PIC X(6)     VALUE SPACES.                 
           03  WS-SALDO      PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99- VALUE ZEROS.   
                                                                        
       01  WS-TIT-SUCURSAL.                                             
           03  FILLER        PIC X(27)    VALUE ALL '*'.                
           03  FILLER        PIC X(26)    VALUE                         
                             '   TOTAL CUENTAS SUCURSAL '.              
           03  WS-SUCNAME    PIC Z9       VALUE SPACES.                 
           03  FILLER        PIC X(4)     VALUE ' AL '.                 
           03  WS-DD         PIC Z9       VALUE ZEROS.                  
           03  FILLER        PIC X        VALUE '-'.                    
           03  WS-MM         PIC Z9       VALUE ZEROS.                  
           03  FILLER        PIC X        VALUE '-'.                    
           03  FILLER        PIC 99       VALUE 20.                     
           03  WS-AA         PIC 99       VALUE ZEROS.                  
           03  FILLER        PIC X(3)     VALUE SPACES.                 
           03  FILLER        PIC X(28)    VALUE ALL '*'.                
                                                                        
       01  WS-TOT-SUCURSAL1.                                            
           03  FILLER        PIC X(49)    VALUE SPACES.                 
           03  FILLER        PIC X(15)    VALUE                         
                                    'TOTAL CUENTAS: '.                  
           03  WS-SUCTOTAL   PIC Z9       VALUE SPACES.                 
                                                                        
       01  WS-TOT-SUCURSAL2.                                            
           03  FILLER        PIC X(49)    VALUE SPACES.                 
           03  FILLER        PIC X(21)    VALUE                         
                                    'SUMATORIA DE SALDOS: '.            
           03  WS-SUCSALDO2  PIC ZZZ.ZZZ.ZZZ.ZZZ.999,99-.               
                                                                        
                                                                        
       PROCEDURE DIVISION.                                              
      *-------------------*                                             
      **************************************                            
      *  CUERPO PRINCIPAL DEL PROGRAMA     *                            
      **************************************                            
       MAIN-PROGRAM.                                                    
      *-------------*                                                   
                                                                        
           PERFORM 1000-I-INICIO                                        
              THRU 1000-F-INICIO                                        
                                                                        
           PERFORM 2000-I-PROCESO                                       
              THRU 2000-F-PROCESO                                       
                                                                        
           PERFORM 9999-I-FINAL                                         
              THRU 9999-F-FINAL                                         
                                                                        
           GOBACK                                                       
           .                                                            
      **************************************                            
      *  CUERPO INICIO APERTURA ARCHIVOS   *                            
      **************************************                            
       1000-I-INICIO.                                                   
      *--------------*                                                  
                                                                        
           ACCEPT WS-FECHA FROM DATE.                                   
           MOVE WS-FECHA-AA TO WS-AA.                                   
           MOVE WS-FECHA-MM TO WS-MM.                                   
           MOVE WS-FECHA-DD TO WS-DD.                                   
                                                                        
           OPEN OUTPUT SALIDA                                           
                                                                        
           IF FS-SAL IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN SALIDA = ' FS-SAL                
              MOVE 9999 TO RETURN-CODE                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT VSAM                                             
                                                                        
           IF FS-VSAM IS NOT EQUAL '00'                                 
              DISPLAY '* ERROR EN OPEN VSAM = ' FS-VSAM                 
              MOVE 9999 TO RETURN-CODE                                  
           END-IF.                                                      
                                                                        
           EXEC SQL                                                     
              OPEN CURSOR1                                              
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT EQUAL ZEROS                                   
              MOVE SQLCODE   TO WS-SQLCODE                              
              DISPLAY '* ERROR OPEN CURSOR 1    = ' WS-SQLCODE          
              MOVE '99' TO FS-SQL                                       
           ELSE                                                         
                                                                        
              EXEC SQL                                                  
                 FETCH CURSOR1                                          
                     INTO :DB-CU-TIPCUEN,                               
                          :DB-CU-NROCUEN,                               
                          :DB-CU-SUCUEN,                                
                          :DB-CU-NROCLI,                                
                          :DB-CU-SALDO,                                 
                          :DB-CU-FECSAL                                 
              END-EXEC                                                  
                                                                        
              EVALUATE TRUE                                             
                 WHEN SQLCODE EQUAL ZEROS                               
                    MOVE '00' TO FS-SQL                                 
                 WHEN SQLCODE EQUAL +100                                
                    MOVE '10' TO FS-SQL                                 
                 WHEN OTHER                                             
                    MOVE SQLCODE TO WS-SQLCODE                          
                    DISPLAY 'ERROR FETCH CURSOR: ' WS-SQLCODE           
                    MOVE '99' TO FS-SQL                                 
              END-EVALUATE                                              
           END-IF                                                       
           .                                                            
       1000-F-INICIO.                                                   
           EXIT.                                                        
                                                                        
      **************************************                            
      *  CUERPO PRINCIPAL DE PROCESOS      *                            
      **************************************                            
       2000-I-PROCESO.                                                  
      *---------------*                                                 
                                                                        
           PERFORM UNTIL SQL-EOF OR SQL-NOK                             
                      OR FS-EOF OR FS-NOK                               
                      OR FS-EOF2 OR FS-NOK2                             
                                                                        
              MOVE DB-CU-SUCUEN TO WS-SUCUEN-ANT                        
              MOVE 0 TO CN-TOT-SUC                                      
              MOVE 0 TO AC-SALSUC                                       
              PERFORM 4100-IMPRIMIR-TIT-SUC                             
                         THRU 4100-F-IMPRIMIR-TIT-SUC                   
                                                                        
      * CORTE A *                                                       
              PERFORM UNTIL SQL-EOF OR SQL-NOK                          
                         OR FS-EOF OR FS-NOK                            
                         OR DB-CU-SUCUEN NOT = WS-SUCUEN-ANT            
                                                                        
                 IF DB-CU-SALDO IS LESS OR EQUAL 0                      
                                                                        
                    EXEC SQL                                            
                       SELECT NOMAPE                                    
                       INTO :DB-CL-NOMAPE                               
                       FROM ITPARUT.TBCURCLI                            
                       WHERE NROCLI = :DB-CU-NROCLI                     
                    END-EXEC                                            
                                                                        
                    EVALUATE TRUE                                       
                      WHEN SQLCODE EQUAL ZEROS                          
                         ADD 1 TO CN-TOT-SUC                            
                         ADD DB-CU-SALDO TO AC-SALSUC                   
                         PERFORM 3000-IMPRIMIR-CUENTA                   
                                 THRU 3000-F-IMPRIMIR-CUENTA            
                      WHEN SQLCODE EQUAL +100                           
                         DISPLAY                                        
                          'CLIENTE NO ENCONTRADO EN MAESTRO CLIENTES: ' 
                                                            WS-NROCLI   
                      WHEN OTHER                                        
                         MOVE SQLCODE TO WS-SQLCODE                     
                         DISPLAY 'ERROR EN CONSULTA SQL: ' WS-SQLCODE   
                         MOVE '99' TO FS-SQL                            
                    END-EVALUATE                                        
                                                                        
                 ELSE                                                   
                    PERFORM 5000-GRABAR-VSAM                            
                            THRU 5000-F-GRABAR-VSAM                     
                 END-IF                                                 
                                                                        
                 EXEC SQL                                               
                    FETCH CURSOR1                                       
                        INTO :DB-CU-TIPCUEN,                            
                             :DB-CU-NROCUEN,                            
                             :DB-CU-SUCUEN,                             
                             :DB-CU-NROCLI,                             
                             :DB-CU-SALDO,                              
                             :DB-CU-FECSAL                              
                 END-EXEC                                               
                                                                        
                 EVALUATE TRUE                                          
                    WHEN SQLCODE EQUAL ZEROS                            
                       CONTINUE                                         
                    WHEN SQLCODE EQUAL +100                             
                       MOVE '10' TO FS-SQL                              
                    WHEN OTHER                                          
                       MOVE SQLCODE TO WS-SQLCODE                       
                       DISPLAY 'ERROR FETCH CURSOR: ' WS-SQLCODE        
                       MOVE '99' TO FS-SQL                              
                 END-EVALUATE                                           
                                                                        
              END-PERFORM                                               
                                                                        
              PERFORM 3200-IMPRIMIR-CORTE-SUC                           
                      THRU 3200-F-IMPRIMIR-CORTE-SUC                    
                                                                        
           END-PERFORM                                                  
           .                                                            
       2000-F-PROCESO.                                                  
           EXIT.                                                        
                                                                        
                                                                        
       3000-IMPRIMIR-CUENTA.                                            
      *---------------------*                                           
                                                                        
           MOVE SPACES TO WS-REG-CLIENTE                                
           MOVE DB-CU-TIPCUEN TO WS-TIPCUEN                             
           MOVE DB-CU-NROCUEN TO WS-NROCUEN                             
           MOVE DB-CL-NOMAPE  TO WS-NOMYAPEL                            
           MOVE DB-CU-NROCLI  TO WS-NROCLI                              
           MOVE DB-CU-SALDO   TO WS-SALDO                               
           WRITE REG-SALIDA FROM WS-REG-CLIENTE.                        
                                                                        
              IF FS-SAL IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE SALIDA = ' FS-SAL             
                MOVE 9999 TO RETURN-CODE                                
              END-IF.                                                   
                                                                        
           ADD 1 TO CN-LINEA.                                           
                                                                        
       3000-F-IMPRIMIR-CUENTA.                                          
           EXIT.                                                        
                                                                        
                                                                        
                                                                        
       3200-IMPRIMIR-CORTE-SUC.                                         
      *------------------------*                                        
                                                                        
           MOVE CN-TOT-SUC    TO WS-SUCTOTAL                            
           MOVE AC-SALSUC     TO WS-SUCSALDO2                           
                                                                        
           WRITE REG-SALIDA FROM WS-SEPARADOR2.                         
                                                                        
              IF FS-SAL IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE SALIDA = ' FS-SAL             
                MOVE 9999 TO RETURN-CODE                                
              END-IF.                                                   
                                                                        
           WRITE REG-SALIDA FROM WS-TOT-SUCURSAL1.                      
                                                                        
              IF FS-SAL IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE SALIDA = ' FS-SAL             
                MOVE 9999 TO RETURN-CODE                                
              END-IF.                                                   
                                                                        
           WRITE REG-SALIDA FROM WS-TOT-SUCURSAL2.                      
                                                                        
              IF FS-SAL IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE SALIDA = ' FS-SAL             
                MOVE 9999 TO RETURN-CODE                                
              END-IF.                                                   
                                                                        
           WRITE REG-SALIDA FROM WS-SEPARADOR2.                         
                                                                        
              IF FS-SAL IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE SALIDA = ' FS-SAL             
                MOVE 9999 TO RETURN-CODE                                
              END-IF.                                                   
                                                                        
       3200-F-IMPRIMIR-CORTE-SUC.                                       
           EXIT.                                                        
                                                                        
                                                                        
                                                                        
       4000-IMPRIMIR-TIT-CLI.                                           
      *----------------------*                                          
                                                                        
           MOVE SPACES TO WS-REG-CLIENTE                                
           MOVE DB-CL-NOMAPE TO WS-NOMYAPEL                             
           MOVE DB-CL-NROCLI TO WS-NROCLI                               
           ADD 1 TO CN-LINEA                                            
                                                                        
           WRITE REG-SALIDA FROM WS-TITULO.                             
                                                                        
           IF FS-SAL IS NOT EQUAL '00'                                  
             DISPLAY '* ERROR EN WRITE SALIDA = ' FS-SAL                
             MOVE 9999 TO RETURN-CODE                                   
           END-IF.                                                      
                                                                        
       4000-F-IMPRIMIR-TIT-CLI.                                         
           EXIT.                                                        
                                                                        
                                                                        
       4100-IMPRIMIR-TIT-SUC.                                           
      *----------------------*                                          
                                                                        
           MOVE DB-CU-SUCUEN TO WS-SUCNAME                              
           ADD 1 TO CN-PAGINA                                           
                                                                        
           WRITE REG-SALIDA FROM WS-TIT-SUCURSAL.                       
              IF FS-SAL IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE SALIDA = ' FS-SAL             
                MOVE 9999 TO RETURN-CODE                                
             END-IF.                                                    
                                                                        
           WRITE REG-SALIDA FROM WS-SEPARADOR.                          
              IF FS-SAL IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE SALIDA = ' FS-SAL             
                MOVE 9999 TO RETURN-CODE                                
             END-IF.                                                    
                                                                        
           MOVE 2 TO CN-LINEA.                                          
                                                                        
           PERFORM 4000-IMPRIMIR-TIT-CLI                                
                   THRU 4000-F-IMPRIMIR-TIT-CLI.                        
                                                                        
                                                                        
       4100-F-IMPRIMIR-TIT-SUC.                                         
           EXIT.                                                        
                                                                        
       5000-GRABAR-VSAM.                                                
      *-----------------*                                               
                                                                        
            MOVE DB-CU-TIPCUEN TO VS-TIPCUEN                            
            MOVE DB-CU-NROCUEN TO VS-NROCUEN                            
            MOVE DB-CU-SUCUEN  TO VS-SUCUEN                             
            MOVE DB-CU-NROCLI  TO VS-NROCLI                             
            MOVE DB-CU-SALDO   TO VS-SALDO                              
            MOVE DB-CU-FECSAL  TO VS-FECSAL                             
                                                                        
            WRITE REG-VSAM FROM VS-GRABACION.                           
                                                                        
            IF FS-VSAM IS NOT EQUAL '00'                                
               DISPLAY '* ERROR EN GRABACION VSAM = ' FS-VSAM           
               MOVE 9999 TO RETURN-CODE                                 
            END-IF.                                                     
                                                                        
       5000-F-GRABAR-VSAM.                                              
           EXIT.                                                        
                                                                        
       9999-I-FINAL.                                                    
      *-------------*                                                   
                                                                        
           EXEC SQL                                                     
              CLOSE CURSOR1                                             
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT EQUAL ZEROS                                   
              MOVE SQLCODE TO WS-SQLCODE                                
              DISPLAY '* ERROR CLOSE CURSOR     = ' WS-SQLCODE          
              MOVE 9999 TO RETURN-CODE                                  
           END-IF.                                                      
                                                                        
           CLOSE SALIDA                                                 
                                                                        
           IF FS-SAL NOT EQUAL '00'                                     
              DISPLAY '* ERROR EN CLOSE SALIDA = ' FS-SAL               
              MOVE 9999 TO RETURN-CODE                                  
           END-IF.                                                      
                                                                        
           CLOSE VSAM                                                   
                                                                        
           IF FS-VSAM NOT EQUAL '00'                                    
              DISPLAY '* ERROR EN CLOSE SALIDA = ' FS-VSAM              
              MOVE 9999 TO RETURN-CODE                                  
           END-IF                                                       
           .                                                            
       9999-F-FINAL.                                                    
           EXIT.                                                        
