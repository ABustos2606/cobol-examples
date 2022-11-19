       IDENTIFICATION DIVISION.                                         
        PROGRAM-ID PGMDB215.                                            
      **********************************************************        
      *                                                        *        
      *               TRABAJO PRACTICO 35                      *        
      *          CHECK-POINT 31 BATCH ACTUALIZADOR             *        
      *                    9-11-2022                           *        
      *                                                        *        
      **********************************************************        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
             SELECT CLIENTE ASSIGN DDCLIEN                              
                    ORGANIZATION IS INDEXED                             
                    ACCESS MODE IS SEQUENTIAL                           
                    RECORD KEY IS KEY-CLI                               
                    FILE STATUS IS FS-CLI.                              
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD CLIENTE.                                                      
                                                                        
       01 REG-CLIENTE.                                                  
          03 KEY-CLI     PIC X(17).                                     
          03 FILLER      PIC X(227).                                    
                                                                        
      **************************************                            
       WORKING-STORAGE SECTION.                                         
      **************************************                            
       77  FS-CLI           PIC XX         VALUE SPACES.                
                                                                        
       01  WS-STATUS-FIN    PIC X.                                      
           88  WS-FIN-LECTURA              VALUE 'Y'.                   
           88  WS-NO-FIN-LECTURA           VALUE 'N'.                   
                                                                        
       77  WS-SQLCODE    PIC +++999 USAGE DISPLAY VALUE ZEROS.          
                                                                        
       77  WS-PGMRUT        PIC X(8)       VALUE 'PGMRUT'.              
                                                                        
       01  WS-FECHA.                                                    
           03  WS-ANIO      PIC 99         VALUE ZEROS.                 
           03  WS-MES       PIC 99         VALUE ZEROS.                 
           03  WS-DIA       PIC 99         VALUE ZEROS.                 
                                                                        
       01  WS-FECHA-FINAL.                                              
           03  WS-SS        PIC 99         VALUE ZEROS.                 
           03  WS-AA        PIC 99         VALUE ZEROS.                 
           03  FILLER       PIC X          VALUE '-'.                   
           03  WS-MM        PIC 99         VALUE ZEROS.                 
           03  FILLER       PIC X          VALUE '-'.                   
           03  WS-DD        PIC 99         VALUE ZEROS.                 
                                                                        
           COPY TBVCLIEN.                                               
                                                                        
       77  WS-TOT-LEI       PIC 999        VALUE ZEROS.                 
       77  WS-TOT-INS       PIC 999        VALUE ZEROS.                 
       77  WS-TOT-ERR       PIC 999        VALUE ZEROS.                 
                                                                        
       77  WS-PRINT         PIC ZZ9        VALUE ZEROS.                 
                                                                        
       01 WS-CLI-NOMAPE     PIC X(30)      VALUE SPACES.                
                                                                        
                                                                        
           EXEC SQL                                                     
             INCLUDE SQLCA                                              
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
             INCLUDE TB99CLIE                                           
           END-EXEC.                                                    
                                                                        
       LINKAGE SECTION.                                                 
       01  LK-FECHA.                                                    
           03  LK-FECHASHORT.                                           
              05  LK-SIGLO     PIC 99.                                  
              05  LK-ANIO      PIC 99.                                  
              05  LK-MES       PIC 99.                                  
              05  LK-DIA       PIC 99.                                  
           03  FILLER       PIC X(22).                                  
                                                                        
                                                                        
      ***************************************************************.  
       PROCEDURE DIVISION USING LK-FECHA.                               
      **************************************                            
      *                                    *                            
      *  CUERPO PRINCIPAL DEL PROGRAMA     *                            
      *                                    *                            
      **************************************                            
       MAIN-PROGRAM.                                                    
                                                                        
           PERFORM 1000-INICIO  THRU   F-1000-INICIO.                   
                                                                        
           PERFORM 2000-PROCESO  THRU  F-2000-PROCESO                   
                   UNTIL WS-FIN-LECTURA.                                
                                                                        
           PERFORM 9999-FINAL    THRU  F-9999-FINAL.                    
                                                                        
       F-MAIN-PROGRAM. GOBACK.                                          
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO INICIO APERTURA ARCHIVOS   *                            
      *                                    *                            
      **************************************                            
       1000-INICIO.                                                     
                                                                        
           SET WS-NO-FIN-LECTURA TO TRUE.                               
                                                                        
           ACCEPT WS-FECHA FROM DATE                                    
                                                                        
           INITIALIZE LK-FECHA                                          
           MOVE 20 TO LK-SIGLO                                          
           MOVE WS-ANIO TO LK-ANIO                                      
           SUBTRACT 1 FROM WS-MES                                       
           MOVE WS-MES TO LK-MES                                        
                                                                        
           CALL  WS-PGMRUT  USING LK-FECHA                              
                                                                        
           IF RETURN-CODE    EQUAL    05                                
             SET WS-FIN-LECTURA TO TRUE                                 
             DISPLAY  'ERROR RUTINA FECHA: 05'                          
           ELSE                                                         
             MOVE LK-SIGLO TO WS-SS                                     
             MOVE LK-ANIO TO WS-AA                                      
             MOVE LK-MES TO WS-MM                                       
             MOVE LK-DIA TO WS-DD                                       
           END-IF.                                                      
                                                                        
           OPEN INPUT CLIENTE.                                          
           IF FS-CLI IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN CLIENTE = ' FS-CLI               
              MOVE 9999 TO RETURN-CODE                                  
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
           PERFORM 3000-LEER-CLIENTE THRU 3000-F-LEER-CLIENTE.          
                                                                        
       F-1000-INICIO.   EXIT.                                           
                                                                        
      **************************************                            
       2000-PROCESO.                                                    
                                                                        
                                                                        
           MOVE WK-CLI-TIPO-DOCUMENTO TO DB-CL-TIPDOC                   
           MOVE WK-CLI-NRO-DOCUMENTO  TO DB-CL-NRODOC                   
           MOVE WK-CLI-NRO-CLIENTE    TO DB-CL-NROCLI                   
           MOVE WS-FECHA-FINAL        TO DB-CL-FECNAC                   
           MOVE WK-CLI-SEXO           TO DB-CL-SEXO                     
                                                                        
           EXEC SQL                                                     
              INSERT INTO ITPLZRY.TB99CLIE                              
                     (TIPDOC,                                           
                      NRODOC,                                           
                      NROCLI,                                           
                      NOMAPE,                                           
                      FECNAC,                                           
                      SEXO)                                             
              VALUES (:DB-CL-TIPDOC,                                    
                      :DB-CL-NRODOC,                                    
                      :DB-CL-NROCLI,                                    
                      :WS-CLI-NOMAPE,                                   
                      :DB-CL-FECNAC,                                    
                      :DB-CL-SEXO)                                      
           END-EXEC.                                                    
                                                                        
           EVALUATE SQLCODE                                             
             WHEN +0                                                    
                ADD 1 TO WS-TOT-INS                                     
             WHEN +100                                                  
                SET WS-FIN-LECTURA TO TRUE                              
             WHEN -803                                                  
                ADD 1 TO WS-TOT-ERR                                     
                DISPLAY 'CLIENTE DUPLICADO'                             
                      WK-CLI-TIPO-DOCUMENTO ' ' WK-CLI-NRO-DOCUMENTO    
             WHEN OTHER                                                 
                ADD 1 TO WS-TOT-ERR                                     
                MOVE SQLCODE TO WS-SQLCODE                              
                DISPLAY 'ERROR ACCESO TABLA: ' WS-SQLCODE               
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
           END-EVALUATE.                                                
                                                                        
           PERFORM 3000-LEER-CLIENTE THRU 3000-F-LEER-CLIENTE.          
                                                                        
                                                                        
       F-2000-PROCESO. EXIT.                                            
                                                                        
                                                                        
      **************************************                            
      * LECTURA CLIENTES                   *                            
      **************************************                            
       3000-LEER-CLIENTE.                                               
                                                                        
           MOVE SPACES TO WK-TBCLIE                                     
           MOVE SPACES TO WS-CLI-NOMAPE                                 
                                                                        
           READ CLIENTE INTO WK-TBCLIE                                  
                                                                        
           EVALUATE FS-CLI                                              
             WHEN '00'                                                  
                ADD 1 TO WS-TOT-LEI                                     
                STRING WK-CLI-NOMBRE-CLIENTE DELIMITED BY '  '          
                       ' ' DELIMITED BY SIZE                            
                       WK-CLI-APELLIDO-CLIENTE DELIMITED BY '  '        
                       INTO WS-CLI-NOMAPE                               
                END-STRING                                              
             WHEN '10'                                                  
                SET WS-FIN-LECTURA TO TRUE                              
             WHEN OTHER                                                 
                DISPLAY '* ERROR EN LECTURA CLIENTE = ' FS-CLI          
                SET WS-FIN-LECTURA TO TRUE                              
                                                                        
           END-EVALUATE.                                                
                                                                        
       3000-F-LEER-CLIENTE. EXIT.                                       
                                                                        
                                                                        
                                                                        
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO FINAL CIERRE DE FILES      *                            
      *                                    *                            
      **************************************                            
       9999-FINAL.                                                      
                                                                        
           MOVE WS-TOT-LEI TO WS-PRINT                                  
           DISPLAY 'TOTAL NOVEDADES LEIDAS     = ' WS-PRINT             
           MOVE WS-TOT-INS TO WS-PRINT                                  
           DISPLAY 'TOTAL NOVEDADES INSERTADAS = ' WS-PRINT             
           MOVE WS-TOT-ERR TO WS-PRINT                                  
           DISPLAY 'TOTAL NOVEDADES ERRONEAS   = ' WS-PRINT             
                                                                        
                                                                        
           CLOSE CLIENTE                                                
              IF FS-CLI IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN CLOSE CLIENTE = '                   
                                            FS-CLI                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
             END-IF.                                                    
                                                                        
      *    EXEC SQL                                                     
      *        ROLLBACK                                                 
      *    END-EXEC.                                                    
                                                                        
       F-9999-FINAL.                                                    
           EXIT.                                                        
      *                                                                 