       IDENTIFICATION DIVISION.                                         
        PROGRAM-ID PGMAPI15.                                            
      **********************************************************        
      *                                                        *        
      *               TRABAJO PRACTICO 27                      *        
      *          CHECK-POINT 14 BATCH - APAREO                 *        
      *                    11-10-2022                          *        
      *                                                        *        
      **********************************************************        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
             SELECT NOVEDAD  ASSIGN DDNOVED                             
                    FILE STATUS IS FS-NOV.                              
                                                                        
             SELECT CODPOS  ASSIGN DDCODPO                              
                    FILE STATUS IS FS-COD                               
                    ORGANIZATION IS INDEXED                             
                    ACCESS MODE IS SEQUENTIAL                           
                    RECORD KEY IS KEY-CODPOS.                           
                                                                        
             SELECT SALIDA  ASSIGN DDSAL                                
                    FILE STATUS IS FS-SAL.                              
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD NOVEDAD                                                       
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
                                                                        
       01 REG-NODEDAD    PIC X(40).                                     
                                                                        
       FD CODPOS.                                                       
                                                                        
       01 REG-CODPOS.                                                   
           03 KEY-CODPOS   PIC X(4).                                    
           03 FILLER       PIC X(66).                                   
                                                                        
       01 REG-CODPOS     PIC X(70).                                     
                                                                        
       FD SALIDA                                                        
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
                                                                        
       01 REG-SALIDA     PIC X(40).                                     
                                                                        
      **************************************                            
       WORKING-STORAGE SECTION.                                         
      **************************************                            
       77  FS-NOV           PIC XX    VALUE SPACES.                     
       77  FS-COD           PIC XX    VALUE SPACES.                     
       77  FS-SAL           PIC XX    VALUE SPACES.                     
                                                                        
       01  WS-STATUS-FIN    PIC X.                                      
           88  WS-FIN-LECTURA         VALUE 'Y'.                        
           88  WS-NO-FIN-LECTURA      VALUE 'N'.                        
                                                                        
       01  WS-STATUS-NOV    PIC X.                                      
           88  WS-FIN-NOV             VALUE 'Y'.                        
           88  WS-NO-FIN-NOV          VALUE 'N'.                        
                                                                        
       01  WS-STATUS-COD    PIC X.                                      
           88  WS-FIN-COD             VALUE 'Y'.                        
           88  WS-NO-FIN-COD          VALUE 'N'.                        
                                                                        
                                                                        
       COPY CPNOVCOD.                                                   
                                                                        
       COPY CPCODPOS.                                                   
                                                                        
       COPY CPNOVCOD REPLACING                                          
            WS-REG-NOVCOD  BY WS-REG-SALIDA                             
            WS-NOVCOD-TD   BY WS-SAL-TD                                 
            WS-NOVCOD-DOC  BY WS-SAL-DOC                                
            WS-NOVCOD-SEXO BY WS-SAL-SEXO                               
            WS-NOVCOD-NRO  BY WS-SAL-NRO.                               
                                                                        
                                                                        
       01  K-NOV-CLAVE   PIC 9(4)    VALUE ZEROS.                       
       01  K-COD-CLAVE   PIC 9(4)    VALUE ZEROS.                       
                                                                        
                                                                        
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
                                                                        
           OPEN INPUT  NOVEDAD.                                         
           IF FS-NOV IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN NOVURSAL = ' FS-NOV              
              MOVE 9999 TO RETURN-CODE                                  
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
           OPEN INPUT  CODPOS.                                          
           IF FS-COD IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN CODIMI  = ' FS-COD               
              MOVE 9999 TO RETURN-CODE                                  
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
           OPEN OUTPUT SALIDA.                                          
           IF FS-SAL IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN SALIDA  = ' FS-SAL               
              MOVE 9999 TO RETURN-CODE                                  
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
            PERFORM 3000-LEER-COD  THRU F-3000-LEER-COD.                
            PERFORM 4000-LEER-NOV  THRU F-4000-LEER-NOV.                
                                                                        
                                                                        
       F-1000-INICIO.   EXIT.                                           
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO PRINCIPAL DE PROCESOS      *                            
      *  LECTURA FILE INPUT CLASIFICADO    *                            
      *  APAREO ARCHIVOS DE ENTRADA        *                            
      *                                    *                            
      **************************************                            
       2000-PROCESO.                                                    
                                                                        
           IF K-NOV-CLAVE EQUAL K-COD-CLAVE                             
             IF WS-CODPOS-JUR EQUAL 'CABA'                              
               PERFORM 6000-GRABAR-SALIDA                               
                          THRU     F-6000-GRABAR-SALIDA                 
             END-IF                                                     
                                                                        
               PERFORM 3000-LEER-COD                                    
                         THRU   F-3000-LEER-COD                         
                                                                        
           ELSE                                                         
                                                                        
             IF K-NOV-CLAVE > K-COD-CLAVE                               
                  PERFORM 3000-LEER-COD                                 
                            THRU   F-3000-LEER-COD                      
             ELSE                                                       
                  PERFORM 4000-LEER-NOV                                 
                            THRU  F-4000-LEER-NOV                       
                                                                        
             END-IF                                                     
                                                                        
           END-IF.                                                      
      *************************************************************     
      * CONTROL FIN DE ARCHIVOS DE ENTRADA, PARA FIN PROGRAMA           
      *************************************************************     
                                                                        
           IF WS-FIN-COD AND WS-FIN-NOV                                 
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
       F-2000-PROCESO. EXIT.                                            
                                                                        
      **************************************                            
      * LECTURA CODIGO POSTAL              *                            
      **************************************                            
       3000-LEER-COD.                                                   
                                                                        
           READ CODPOS  INTO WS-REG-CODPOS                              
                        AT END SET WS-FIN-COD  TO TRUE                  
                                MOVE 9999 TO K-COD-CLAVE.               
                                                                        
           EVALUATE FS-COD                                              
             WHEN '00'                                                  
                  MOVE WS-CODPOS-NRO TO K-COD-CLAVE                     
              WHEN '10'                                                 
              SET WS-FIN-COD  TO TRUE                                   
                                                                        
           WHEN OTHER                                                   
              DISPLAY '* ERROR EN LECTURA CODPOS  = ' FS-COD            
              MOVE 9999 TO RETURN-CODE                                  
              SET WS-FIN-COD  TO TRUE                                   
                                                                        
           END-EVALUATE.                                                
                                                                        
       F-3000-LEER-COD. EXIT.                                           
                                                                        
      **************************************                            
      * LECTURA NOVEDADES                  *                            
      **************************************                            
       4000-LEER-NOV.                                                   
                                                                        
           READ NOVEDAD INTO WS-REG-NOVCOD                              
                        AT END SET WS-FIN-NOV TO TRUE                   
                                MOVE 9999 TO K-NOV-CLAVE.               
                                                                        
           EVALUATE FS-NOV                                              
             WHEN '00'                                                  
                  MOVE WS-NOVCOD-NRO TO K-NOV-CLAVE                     
              WHEN '10'                                                 
              SET WS-FIN-NOV     TO TRUE                                
                                                                        
           WHEN OTHER                                                   
              DISPLAY '* ERROR EN LECTURA NOVURSAL = ' FS-NOV           
              MOVE 9999 TO RETURN-CODE                                  
              SET WS-FIN-NOV     TO TRUE                                
                                                                        
           END-EVALUATE.                                                
                                                                        
       F-4000-LEER-NOV. EXIT.                                           
                                                                        
                                                                        
      ***************************************************               
      *PARRAFO PARA GRABAR LA SALIDA ACTUALIZADA                        
      ***************************************************               
                                                                        
       6000-GRABAR-SALIDA.                                              
                                                                        
             MOVE WS-NOVCOD-TD   TO WS-SAL-TD                           
             MOVE WS-NOVCOD-DOC  TO WS-SAL-DOC                          
             MOVE WS-NOVCOD-SEXO TO WS-SAL-SEXO                         
             MOVE WS-NOVCOD-NRO  TO WS-SAL-NRO                          
                                                                        
             WRITE REG-SALIDA   FROM WS-REG-SALIDA.                     
                                                                        
             IF FS-SAL = ZEROS                                          
                  CONTINUE                                              
             ELSE                                                       
                  DISPLAY '* ERROR EN WRITE SALIDA  = '                 
                                            FS-SAL                      
                  MOVE 9999 TO RETURN-CODE                              
                  SET WS-FIN-LECTURA TO TRUE                            
             END-IF.                                                    
                                                                        
       F-6000-GRABAR-SALIDA. EXIT.                                      
                                                                        
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO FINAL CIERRE DE FILES      *                            
      *                                    *                            
      **************************************                            
       9999-FINAL.                                                      
                                                                        
           CLOSE NOVEDAD                                                
              IF FS-NOV IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN CLOSE NOVURSAL = '                  
                                            FS-NOV                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
             END-IF.                                                    
                                                                        
           CLOSE  CODPOS                                                
              IF FS-COD IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN CLOSE CODIMI   ='                   
                                            FS-COD                      
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
                                                                        
                                                                        
       F-9999-FINAL.                                                    
           EXIT.                                                        
      *                                                                 