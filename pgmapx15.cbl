       IDENTIFICATION DIVISION.                                         
        PROGRAM-ID PGMAPX15.                                            
      **********************************************************        
      *                                                        *        
      *               TRABAJO PRACTICO 27 - B                  *        
      *          CHECK-POINT 14 BATCH - APAREO                 *        
      *                    12-10-2022                          *        
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
                    ACCESS MODE IS RANDOM                               
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
              DISPLAY '* ERROR EN OPEN NOVEDAD = ' FS-NOV               
              MOVE 9999 TO RETURN-CODE                                  
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
           OPEN INPUT  CODPOS.                                          
           IF FS-COD IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN CODPOS  = ' FS-COD               
              MOVE 9999 TO RETURN-CODE                                  
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
           OPEN OUTPUT SALIDA.                                          
           IF FS-SAL IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN SALIDA  = ' FS-SAL               
              MOVE 9999 TO RETURN-CODE                                  
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
                                                                        
       F-1000-INICIO.   EXIT.                                           
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO PRINCIPAL DE PROCESOS      *                            
      *                                    *                            
      **************************************                            
       2000-PROCESO.                                                    
                                                                        
           PERFORM 4000-LEER-NOV  THRU F-4000-LEER-NOV                  
           MOVE WS-NOVCOD-NRO TO KEY-CODPOS                             
           READ CODPOS  INTO WS-REG-CODPOS                              
           EVALUATE FS-COD                                              
              WHEN '00'                                                 
                 IF WS-CODPOS-JUR EQUAL 'CABA'                          
                      PERFORM 6000-GRABAR-SALIDA                        
                            THRU F-6000-GRABAR-SALIDA                   
                 END-IF                                                 
              WHEN '23'                                                 
                 CONTINUE                                               
                                                                        
              WHEN OTHER                                                
                 DISPLAY 'ERROR EN ARCHIVO VSAM ' FS-COD                
                 SET WS-FIN-LECTURA TO TRUE                             
           END-EVALUATE                                                 
                                                                        
                                                                        
                                                                        
                                                                        
      *************************************************************     
      * CONTROL FIN DE ARCHIVOS DE ENTRADA, PARA FIN PROGRAMA           
      *************************************************************     
                                                                        
           IF WS-FIN-NOV                                                
              SET  WS-FIN-LECTURA TO TRUE                               
           END-IF.                                                      
                                                                        
       F-2000-PROCESO. EXIT.                                            
                                                                        
                                                                        
      **************************************                            
      * LECTURA NOVEDADES                  *                            
      **************************************                            
       4000-LEER-NOV.                                                   
                                                                        
           READ NOVEDAD INTO WS-REG-NOVCOD                              
                                                                        
           EVALUATE FS-NOV                                              
             WHEN '00'                                                  
                  CONTINUE                                              
              WHEN '10'                                                 
              SET WS-FIN-NOV     TO TRUE                                
                                                                        
           WHEN OTHER                                                   
              DISPLAY '* ERROR EN LECTURA NOVEDAD = ' FS-NOV            
              MOVE 9999 TO RETURN-CODE                                  
              SET WS-FIN-NOV     TO TRUE                                
                                                                        
           END-EVALUATE.                                                
                                                                        
       F-4000-LEER-NOV. EXIT.                                           
                                                                        
                                                                        
      ***************************************************               
      *PARRAFO PARA GRABAR LA SALIDA ACTUALIZADA                        
      ***************************************************               
                                                                        
       6000-GRABAR-SALIDA.                                              
                                                                        
             MOVE WS-REG-NOVCOD TO WS-REG-SALIDA                        
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
                DISPLAY '* ERROR EN CLOSE NOVEDAD = '                   
                                            FS-NOV                      
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-LECTURA TO TRUE                              
             END-IF.                                                    
                                                                        
           CLOSE  CODPOS                                                
              IF FS-COD IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN CLOSE CODPOS   ='                   
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