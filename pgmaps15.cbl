       IDENTIFICATION DIVISION.                                         
        PROGRAM-ID PGMAPS15.                                            
      **********************************************************        
      *                                                        *        
      *               TRABAJO PRACTICO 26                      *        
      *          CHECK-POINT 12 BATCH - APAREO                 *        
      *                    6-10-2022                           *        
      *                                                        *        
      **********************************************************        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
             SELECT SUCURSAL ASSIGN DDSUCUR                             
                    FILE STATUS IS FS-SUC.                              
                                                                        
             SELECT MOVIMI  ASSIGN DDMOVIM                              
                    FILE STATUS IS FS-MOV.                              
                                                                        
             SELECT SALIDA  ASSIGN DDSAL                                
                    FILE STATUS IS FS-SAL.                              
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD SUCURSAL                                                      
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
                                                                        
       01 REG-SUCURSAL   PIC X(93).                                     
                                                                        
       FD MOVIMI                                                        
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
                                                                        
       01 REG-MOVIMI     PIC X(93).                                     
                                                                        
       FD SALIDA                                                        
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
                                                                        
       01 REG-SALIDA     PIC X(93).                                     
                                                                        
      **************************************                            
       WORKING-STORAGE SECTION.                                         
      **************************************                            
       77  FS-SUC           PIC XX    VALUE SPACES.                     
       77  FS-MOV           PIC XX    VALUE SPACES.                     
       77  FS-SAL           PIC XX    VALUE SPACES.                     
                                                                        
       01  WS-STATUS-FIN    PIC X.                                      
           88  WS-FIN-LECTURA         VALUE 'Y'.                        
           88  WS-NO-FIN-LECTURA      VALUE 'N'.                        
                                                                        
       01  WS-STATUS-SUC    PIC X.                                      
           88  WS-FIN-SUC             VALUE 'Y'.                        
           88  WS-NO-FIN-SUC          VALUE 'N'.                        
                                                                        
       01  WS-STATUS-MOV    PIC X.                                      
           88  WS-FIN-MOV             VALUE 'Y'.                        
           88  WS-NO-FIN-MOV          VALUE 'N'.                        
                                                                        
       01  WS-REG-SUCUR.                                                
           COPY CPSUCUR.                                                
                                                                        
       01  WS-REG-MOVIM.                                                
           COPY CPSUCUR REPLACING                                       
              WS-REG-SUCURSAL  BY WS-REG-MOV                            
              WS-SUC-NRO       BY WS-MOV-NRO                            
              WS-SUC-TIP-DOC   BY WS-MOV-TIP-DOC                        
              WS-SUC-NRO-DOC   BY WS-MOV-NRO-DOC                        
              WS-SUC-NOMAPE    BY WS-MOV-NOMAPE                         
              WS-SUC-EST-CIV   BY WS-MOV-EST-CIV                        
              WS-SUC-SEXO      BY WS-MOV-SEXO                           
              WS-SUC-CUENTA    BY WS-MOV-CUENTA                         
              WS-SUC-TIP       BY WS-MOV-TIP                            
              WS-SUC-CTA       BY WS-MOV-CTA                            
              WS-SUC-SALDO     BY WS-MOV-SALDO.                         
                                                                        
       01  WS-REG-SALIDA.                                               
           COPY CPSUCUR REPLACING                                       
              WS-REG-SUCURSAL  BY WS-REG-SAL                            
              WS-SUC-NRO       BY WS-SAL-NRO                            
              WS-SUC-TIP-DOC   BY WS-SAL-TIP-DOC                        
              WS-SUC-NRO-DOC   BY WS-SAL-NRO-DOC                        
              WS-SUC-NOMAPE    BY WS-SAL-NOMAPE                         
              WS-SUC-EST-CIV   BY WS-SAL-EST-CIV                        
              WS-SUC-SEXO      BY WS-SAL-SEXO                           
              WS-SUC-CUENTA    BY WS-SAL-CUENTA                         
              WS-SUC-TIP       BY WS-SAL-TIP                            
              WS-SUC-CTA       BY WS-SAL-CTA                            
              WS-SUC-SALDO     BY WS-SAL-SALDO.                         
                                                                        
       77  WS-SUC-TOT-LEIDOS    PIC 99    VALUE ZEROS.                  
       77  WS-MOV-TOT-LEIDOS    PIC 99    VALUE ZEROS.                  
       77  WS-SAL-TOT-GRAB      PIC 99    VALUE ZEROS.                  
       77  WS-TOT-ENCONTRADO    PIC 99    VALUE ZEROS.                  
       77  WS-TOT-NOENCONTRADO  PIC 99    VALUE ZEROS.                  
                                                                        
       01  K-SUC-CLAVE.                                                 
           03  K-SUC-NRO        PIC 9(3)  VALUE ZEROS.                  
           03  K-SUC-CUENTA     PIC 9(5)  VALUE ZEROS.                  
                                                                        
       01  K-MOV-CLAVE.                                                 
           03  K-MOV-NRO        PIC 9(3)  VALUE ZEROS.                  
           03  K-MOV-CUENTA     PIC 9(5)  VALUE ZEROS.                  
                                                                        
       77  WS-TOT-PRINT         PIC ZZ9   VALUE SPACES.                 
                                                                        
      ***************************************************************.  
       PROCEDURE DIVISION.                                              
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
                                                                        
           OPEN INPUT  SUCURSAL.                                        
           IF FS-SUC IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN SUCURSAL = ' FS-SUC              
              MOVE 9999 TO RETURN-CODE                                  
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
           OPEN INPUT  MOVIMI.                                          
           IF FS-MOV IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN MOVIMI  = ' FS-MOV               
              MOVE 9999 TO RETURN-CODE                                  
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
           OPEN OUTPUT SALIDA.                                          
           IF FS-SAL IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN SALIDA  = ' FS-SAL               
              MOVE 9999 TO RETURN-CODE                                  
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
            PERFORM 3000-LEER-MOVI  THRU F-3000-LEER-MOVI.              
            PERFORM 4000-LEER-SUC  THRU F-4000-LEER-SUC.                
                                                                        
                                                                        
       F-1000-INICIO.   EXIT.                                           
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO PRINCIPAL DE PROCESOS      *                            
      *  LECTURA FILE INPUT CLASIFICADO    *                            
      *  APAREO ARCHIVOS DE ENTRADA        *                            
      *                                    *                            
      **************************************                            
       2000-PROCESO.                                                    
                                                                        
           IF K-SUC-CLAVE EQUAL K-MOV-CLAVE                             
               ADD 1 TO WS-TOT-ENCONTRADO                               
               PERFORM 5000-PROCESAR-SUCURSAL                           
                          THRU     F-5000-PROCESAR-SUCURSAL             
                                                                        
               PERFORM 3000-LEER-MOVI                                   
                         THRU   F-3000-LEER-MOVI                        
                                                                        
           ELSE                                                         
                                                                        
             IF K-SUC-CLAVE > K-MOV-CLAVE                               
                  ADD 1 TO WS-TOT-NOENCONTRADO                          
                  PERFORM 3000-LEER-MOVI                                
                            THRU   F-3000-LEER-MOVI                     
             ELSE                                                       
                  PERFORM 6000-GRABAR-SALIDA                            
                            THRU F-6000-GRABAR-SALIDA                   
                  PERFORM 4000-LEER-SUC                                 
                            THRU  F-4000-LEER-SUC                       
                                                                        
             END-IF                                                     
                                                                        
           END-IF.                                                      
      *************************************************************     
      * CONTROL FIN DE ARCHIVOS DE ENTRADA, PARA FIN PROGRAMA           
      *************************************************************     
                                                                        
           IF WS-FIN-MOV AND WS-FIN-SUC                                 
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
       F-2000-PROCESO. EXIT.                                            
                                                                        
      **************************************                            
      * LECTURA MOVIMIENTOS                *                            
      **************************************                            
       3000-LEER-MOVI.                                                  
                                                                        
           READ MOVIMI  INTO WS-REG-MOVIM                               
                        AT END SET WS-FIN-MOV  TO TRUE                  
                                MOVE HIGH-VALUE TO K-MOV-CLAVE.         
                                                                        
           EVALUATE FS-MOV                                              
             WHEN '00'                                                  
                  MOVE WS-MOV-NRO TO K-MOV-NRO                          
                  MOVE WS-MOV-CUENTA TO K-MOV-CUENTA                    
                  ADD 1 TO WS-MOV-TOT-LEIDOS                            
              WHEN '10'                                                 
              SET WS-FIN-MOV  TO TRUE                                   
                                                                        
           WHEN OTHER                                                   
              DISPLAY '* ERROR EN LECTURA MOVIMI  = ' FS-MOV            
              MOVE 9999 TO RETURN-CODE                                  
              SET WS-FIN-MOV  TO TRUE                                   
                                                                        
           END-EVALUATE.                                                
                                                                        
       F-3000-LEER-MOVI. EXIT.                                          
                                                                        
      **************************************                            
      * LECTURA SUCURSAL                   *                            
      **************************************                            
       4000-LEER-SUC.                                                   
                                                                        
           READ SUCURSAL INTO WS-REG-SUCURSAL                           
                        AT END SET WS-FIN-SUC TO TRUE                   
                                MOVE HIGH-VALUE TO K-SUC-CLAVE.         
                                                                        
           EVALUATE FS-SUC                                              
             WHEN '00'                                                  
                  MOVE WS-SUC-NRO TO K-SUC-NRO                          
                  MOVE WS-SUC-CUENTA TO K-SUC-CUENTA                    
                  ADD 1 TO WS-SUC-TOT-LEIDOS                            
              WHEN '10'                                                 
              SET WS-FIN-SUC     TO TRUE                                
                                                                        
           WHEN OTHER                                                   
              DISPLAY '* ERROR EN LECTURA SUCURSAL = ' FS-SUC           
              MOVE 9999 TO RETURN-CODE                                  
              SET WS-FIN-SUC     TO TRUE                                
                                                                        
           END-EVALUATE.                                                
                                                                        
       F-4000-LEER-SUC. EXIT.                                           
                                                                        
       5000-PROCESAR-SUCURSAL.                                          
                                                                        
           ADD WS-MOV-SALDO TO WS-SAL-SALDO.                            
                                                                        
       F-5000-PROCESAR-SUCURSAL. EXIT.                                  
                                                                        
      ***************************************************               
      *PARRAFO PARA GRABAR LA SALIDA ACTUALIZADA                        
      ***************************************************               
                                                                        
       6000-GRABAR-SALIDA.                                              
                                                                        
           IF WS-SAL-SALDO NOT EQUAL ZERO                               
             ADD WS-SAL-SALDO TO WS-SUC-SALDO                           
             ADD 1 TO WS-SAL-TOT-GRAB                                   
             WRITE REG-SALIDA   FROM WS-REG-SUCURSAL                    
                                                                        
             IF FS-SAL =       ZEROS                                    
                  CONTINUE                                              
             ELSE                                                       
                  DISPLAY '* ERROR EN WRITE SALIDA  = '                 
                                            FS-SAL                      
                  MOVE 9999 TO RETURN-CODE                              
                  SET WS-FIN-LECTURA TO TRUE                            
             END-IF                                                     
           END-IF.                                                      
           MOVE 0 TO WS-SAL-SALDO.                                      
                                                                        
       F-6000-GRABAR-SALIDA. EXIT.                                      
                                                                        
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO FINAL CIERRE DE FILES      *                            
      *                                    *                            
      **************************************                            
       9999-FINAL.                                                      
                                                                        
           CLOSE SUCURSAL                                               
              IF FS-SUC IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN CLOSE SUCURSAL = '                  
                                            FS-SUC                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
             END-IF.                                                    
                                                                        
           CLOSE  MOVIMI                                                
              IF FS-MOV IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN CLOSE MOVIMI   ='                   
                                            FS-MOV                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
           END-IF.                                                      
                                                                        
           CLOSE SALIDA                                                 
              IF FS-SAL IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN CLOSE SALIDA  = '                   
                                            FS-SAL                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
             END-IF.                                                    
                                                                        
           MOVE WS-SUC-TOT-LEIDOS TO WS-TOT-PRINT                       
           DISPLAY 'CLIENTES LEIDOS = ' WS-TOT-PRINT                    
           MOVE WS-MOV-TOT-LEIDOS TO WS-TOT-PRINT                       
           DISPLAY 'MOVIMIENTOS LEIDOS = ' WS-TOT-PRINT                 
           MOVE WS-SAL-TOT-GRAB TO WS-TOT-PRINT                         
           DISPLAY 'CLIENTES CON SALDO ACTUALIZADO = ' WS-TOT-PRINT     
           MOVE WS-TOT-ENCONTRADO TO WS-TOT-PRINT                       
           DISPLAY 'REGISTROS ENCONTRADOS = ' WS-TOT-PRINT              
           MOVE WS-TOT-NOENCONTRADO TO WS-TOT-PRINT                     
           DISPLAY 'REGISTROS NO ENCONTRADOS = ' WS-TOT-PRINT.          
                                                                        
                                                                        
                                                                        
                                                                        
       F-9999-FINAL.                                                    
           EXIT.                                                        
      *                                                                 