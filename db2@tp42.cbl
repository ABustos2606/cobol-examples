       IDENTIFICATION DIVISION.                                         
      *------------------------*                                        
       PROGRAM-ID. PGMDB215.                                            
      ********************************************************          
      *                                                      *          
      *           TP 42 - CHECK-POINT 33 BATCH DB2           *          
      *                     16-11-2022                       *          
      *                                                      *          
      ********************************************************          
       ENVIRONMENT DIVISION.                                            
      *---------------------*                                           
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT SALIDA ASSIGN TO DDSALID                              
           FILE STATUS IS FS-SAL.                                       
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  SALIDA                                                       
           BLOCK CONTAINS  0 RECORDS                                    
           RECORDING MODE IS F.                                         
                                                                        
       01  REG-SALIDA         PIC X(132).                               
                                                                        
       WORKING-STORAGE SECTION.                                         
      *------------------------*                                        
                                                                        
       01  FS-SAL             PIC X(02).                                
           88  FS-OK                      VALUE '00'.                   
           88  FS-EOF                     VALUE '10'.                   
           88  FS-NOK                     VALUE '01' THRU '09'          
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
             SELECT A.TIPCUEN,                                          
                    A.NROCUEN,                                          
                    A.SUCUEN,                                           
                    A.NROCLI,                                           
                    A.SALDO,                                            
                    B.TIPDOC,                                           
                    B.NRODOC,                                           
                    B.NROCLI,                                           
                    B.NOMAPE                                            
             FROM ITPARUT.TBCURCTA A                                    
               INNER JOIN ITPARUT.TBCURCLI B                            
               ON A.NROCLI = B.NROCLI                                   
               ORDER BY A.SUCUEN ASC,                                   
                        A.TIPCUEN ASC                                   
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
           03  FILLER        PIC X(57)    VALUE ALL '*'.                
           03  FILLER        PIC X(12)    VALUE '   SUCURSAL '.         
           03  WS-SUCNAME    PIC Z9       VALUE SPACES.                 
           03  FILLER        PIC X(3)     VALUE SPACES.                 
           03  FILLER        PIC X(38)    VALUE ALL '*'.                
           03  FILLER        PIC X(3)     VALUE SPACES.                 
           03  FILLER        PIC X(8)     VALUE 'PAGINA: '.             
           03  WS-PAGINA     PIC Z9       VALUE ZEROS.                  
           03  FILLER        PIC X(3)     VALUE SPACES.                 
           03  FILLER        PIC X(4)     VALUE ALL '*'.                
                                                                        
       01  WS-TOT-CUENTA.                                               
           03  FILLER        PIC X(45)    VALUE SPACES.                 
           03  FILLER        PIC X(25)    VALUE                         
                                    'CANTIDAD DE CUENTAS TIPO '.        
           03  WS-CUETIPO    PIC XX       VALUE SPACES.                 
           03  FILLER        PIC X(3)     VALUE ' = '.                  
           03  WS-CUETOTAL   PIC Z9       VALUE SPACES.                 
           03  FILLER        PIC X(13)    VALUE SPACES.                 
           03  FILLER        PIC X(15)    VALUE 'TOTAL SALDOS = '.      
           03  WS-SUCSALDO   PIC ZZZ.ZZZ.ZZZ.ZZZ.999,99-.               
                                                                        
       01  WS-TOT-SUCURSAL.                                             
           03  FILLER        PIC X(45)    VALUE SPACES.                 
           03  FILLER        PIC X(32)    VALUE                         
                                    'CANTIDAD DE CUENTAS EN SUCURSAL '. 
           03  WS-SUCNAME2   PIC Z9       VALUE SPACES.                 
           03  FILLER        PIC X(3)     VALUE ' = '.                  
           03  WS-SUCTOTAL   PIC Z9       VALUE SPACES.                 
           03  FILLER        PIC X(6)     VALUE SPACES.                 
           03  FILLER        PIC X(15)    VALUE 'TOTAL SALDOS = '.      
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
                                                                        
           OPEN OUTPUT SALIDA                                           
                                                                        
           IF FS-SAL IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN SALIDA = ' FS-SAL                
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
                          :DB-CL-TIPDOC,                                
                          :DB-CL-NRODOC,                                
                          :DB-CL-NROCLI,                                
                          :DB-CL-NOMAPE                                 
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
                                                                        
                                                                        
              MOVE DB-CU-SUCUEN TO WS-SUCUEN-ANT                        
              MOVE 0 TO CN-TOT-SUC                                      
              MOVE 0 TO AC-SALSUC                                       
              PERFORM 4100-IMPRIMIR-TIT-SUC                             
                         THRU 4100-F-IMPRIMIR-TIT-SUC                   
                                                                        
      * CORTE A *                                                       
              PERFORM UNTIL SQL-EOF OR SQL-NOK                          
                         OR FS-EOF OR FS-NOK                            
                         OR DB-CU-SUCUEN NOT = WS-SUCUEN-ANT            
                                                                        
                  MOVE DB-CU-TIPCUEN TO WS-TIPCUEN-ANT                  
                  MOVE 0 TO CN-TOT-TIPO                                 
                  MOVE 0 TO AC-SALCUEN                                  
                                                                        
      * CORTE B *                                                       
                  PERFORM UNTIL SQL-EOF OR SQL-NOK                      
                             OR FS-EOF OR FS-NOK                        
                             OR DB-CU-SUCUEN  NOT = WS-SUCUEN-ANT       
                             OR DB-CU-TIPCUEN NOT = WS-TIPCUEN-ANT      
                                                                        
                    ADD 1 TO CN-TOT-TIPO                                
                    ADD DB-CU-SALDO TO AC-SALCUEN                       
                                                                        
                    PERFORM 3000-IMPRIMIR-CUENTA                        
                            THRU 3000-F-IMPRIMIR-CUENTA                 
                                                                        
                    EXEC SQL                                            
                       FETCH CURSOR1                                    
                           INTO :DB-CU-TIPCUEN,                         
                                :DB-CU-NROCUEN,                         
                                :DB-CU-SUCUEN,                          
                                :DB-CU-NROCLI,                          
                                :DB-CU-SALDO,                           
                                :DB-CL-TIPDOC,                          
                                :DB-CL-NRODOC,                          
                                :DB-CL-NROCLI,                          
                                :DB-CL-NOMAPE                           
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
                                                                        
                  PERFORM 3100-IMPRIMIR-CORTE-TIPO                      
                          THRU 3100-F-IMPRIMIR-CORTE-TIPO               
                  ADD CN-TOT-TIPO TO CN-TOT-SUC                         
                  ADD AC-SALCUEN TO AC-SALSUC                           
                                                                        
              END-PERFORM                                               
                                                                        
              PERFORM 3200-IMPRIMIR-CORTE-SUC                           
                      THRU 3200-F-IMPRIMIR-CORTE-SUC                    
              ADD CN-TOT-SUC TO CN-TOT-CLI                              
              ADD AC-SALSUC  TO AC-SALCLI                               
                                                                        
           END-PERFORM                                                  
           .                                                            
       2000-F-PROCESO.                                                  
           EXIT.                                                        
                                                                        
                                                                        
       3000-IMPRIMIR-CUENTA.                                            
      *---------------------*                                           
                                                                        
           IF CN-LINEA GREATER 29                                       
               PERFORM 4100-IMPRIMIR-TIT-SUC                            
                       THRU 4100-F-IMPRIMIR-TIT-SUC                     
           END-IF.                                                      
                                                                        
           MOVE SPACES TO WS-REG-CLIENTE                                
           MOVE DB-CU-TIPCUEN TO WS-TIPCUEN                             
           MOVE DB-CU-NROCUEN TO WS-NROCUEN                             
           MOVE DB-CL-NOMAPE  TO WS-NOMYAPEL                            
           MOVE DB-CL-NROCLI  TO WS-NROCLI                              
           MOVE DB-CU-SALDO   TO WS-SALDO                               
           WRITE REG-SALIDA FROM WS-REG-CLIENTE AFTER 1.                
                                                                        
              IF FS-SAL IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE SALIDA = ' FS-SAL             
                MOVE 9999 TO RETURN-CODE                                
              END-IF.                                                   
                                                                        
           ADD 1 TO CN-LINEA.                                           
                                                                        
       3000-F-IMPRIMIR-CUENTA.                                          
           EXIT.                                                        
                                                                        
                                                                        
       3100-IMPRIMIR-CORTE-TIPO.                                        
      *-------------------------*                                       
                                                                        
           IF CN-LINEA GREATER 29                                       
               PERFORM 4100-IMPRIMIR-TIT-SUC                            
                       THRU 4100-F-IMPRIMIR-TIT-SUC                     
           END-IF.                                                      
                                                                        
           MOVE WS-TIPCUEN-ANT TO WS-CUETIPO                            
           MOVE CN-TOT-TIPO    TO WS-CUETOTAL                           
           MOVE AC-SALCUEN     TO WS-SUCSALDO                           
                                                                        
           WRITE REG-SALIDA FROM WS-TOT-CUENTA AFTER 1.                 
                                                                        
              IF FS-SAL IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE SALIDA = ' FS-SAL             
                MOVE 9999 TO RETURN-CODE                                
              END-IF.                                                   
                                                                        
           ADD 1 TO CN-LINEA.                                           
                                                                        
       3100-F-IMPRIMIR-CORTE-TIPO.                                      
           EXIT.                                                        
                                                                        
                                                                        
       3200-IMPRIMIR-CORTE-SUC.                                         
      *------------------------*                                        
                                                                        
           IF CN-LINEA GREATER 29                                       
               PERFORM 4100-IMPRIMIR-TIT-SUC                            
                       THRU 4100-F-IMPRIMIR-TIT-SUC                     
           END-IF.                                                      
                                                                        
           MOVE WS-SUCUEN-ANT TO WS-SUCNAME2                            
           MOVE CN-TOT-SUC    TO WS-SUCTOTAL                            
           MOVE AC-SALSUC     TO WS-SUCSALDO2                           
                                                                        
           WRITE REG-SALIDA FROM WS-TOT-SUCURSAL AFTER 1.               
                                                                        
              IF FS-SAL IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE SALIDA = ' FS-SAL             
                MOVE 9999 TO RETURN-CODE                                
              END-IF.                                                   
                                                                        
           ADD 1 TO CN-LINEA.                                           
                                                                        
       3200-F-IMPRIMIR-CORTE-SUC.                                       
           EXIT.                                                        
                                                                        
                                                                        
                                                                        
       4000-IMPRIMIR-TIT-CLI.                                           
      *----------------------*                                          
                                                                        
           MOVE SPACES TO WS-REG-CLIENTE                                
           MOVE DB-CL-NOMAPE TO WS-NOMYAPEL                             
           MOVE DB-CL-NROCLI TO WS-NROCLI                               
           ADD 1 TO CN-LINEA                                            
                                                                        
           WRITE REG-SALIDA FROM WS-TITULO AFTER 1                      
                                                                        
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
           MOVE CN-PAGINA TO WS-PAGINA                                  
                                                                        
           WRITE REG-SALIDA FROM WS-TIT-SUCURSAL AFTER PAGE.            
              IF FS-SAL IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE SALIDA = ' FS-SAL             
                MOVE 9999 TO RETURN-CODE                                
             END-IF.                                                    
                                                                        
           WRITE REG-SALIDA FROM WS-SEPARADOR AFTER 1.                  
              IF FS-SAL IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN WRITE SALIDA = ' FS-SAL             
                MOVE 9999 TO RETURN-CODE                                
             END-IF.                                                    
                                                                        
           MOVE 2 TO CN-LINEA.                                          
                                                                        
           PERFORM 4000-IMPRIMIR-TIT-CLI                                
                   THRU 4000-F-IMPRIMIR-TIT-CLI.                        
                                                                        
                                                                        
       4100-F-IMPRIMIR-TIT-SUC.                                         
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
           END-IF                                                       
           .                                                            
       9999-F-FINAL.                                                    
           EXIT.                                                        