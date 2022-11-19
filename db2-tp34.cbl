       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID PGMDB215.                                             
      **********************************************************        
      *                                                        *        
      *              PROGRAMA PARA SQL EMBEBIDO                *        
      *         CHECK-POINT 28 BATCH ACT DB2 - TP 34           *        
      *                       7-11-22                          *        
      *                                                        *        
      **********************************************************        
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
      **************************************                            
       WORKING-STORAGE SECTION.                                         
      **************************************                            
       77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.  
                                                                        
       77  FS-ENT           PIC XX    VALUE SPACES.                     
       01  WS-FLAG-FIN      PIC X.                                      
           88  WS-SI-PROCESO          VALUE ' '.                        
           88  WS-FIN-PROCESO         VALUE 'F'.                        
                                                                        
       77  FILLER        PIC X(26)    VALUE '* VARIABLES SQL       *'.  
       77  WS-SQLCODE    PIC +++999 USAGE DISPLAY VALUE ZEROS.          
                                                                        
       01  WS-STATUS     PIC X.                                         
           88  WS-SI                  VALUE ' '.                        
           88  WS-NO                  VALUE 'F'.                        
                                                                        
       01  WS-TIPDOC     PIC X(2)     VALUE SPACES.                     
       01  WS-NRODOC     PIC S9(11)V USAGE COMP-3  VALUE ZEROS.         
                                                                        
            EXEC SQL                                                    
              INCLUDE SQLCA                                             
            END-EXEC.                                                   
                                                                        
            EXEC SQL                                                    
              INCLUDE TB99CUEN                                          
            END-EXEC.                                                   
                                                                        
            EXEC SQL                                                    
              INCLUDE TB99CLIE                                          
            END-EXEC.                                                   
                                                                        
            EXEC SQL                                                    
              DECLARE CURSOR1 CURSOR FOR                                
              SELECT A.TIPCUEN, A.NROCUEN, A.SUCUEN,                    
                     B.NOMAPE,  B.TIPDOC,  B.NRODOC                     
              FROM  ITPLZRY.TB99CUEN AS A                               
                    RIGHT JOIN                                          
                    ITPLZRY.TB99CLIE AS B                               
                    ON  A.NROCLI = B.NROCLI                             
                    WHERE A.NROCLI = 151                                
                    AND   A.SUCUEN = 1                                  
            END-EXEC.                                                   
                                                                        
       77  FILLER        PIC X(26) VALUE '* FINAL  WORKING-STORAGE *'.  
                                                                        
      ***************************************************************.  
       PROCEDURE DIVISION.                                              
      **************************************                            
      *                                    *                            
      *  CUERPO PRINCIPAL DEL PROGRAMA     *                            
      *                                    *                            
      **************************************                            
       MAIN-PROGRAM.                                                    
                                                                        
           PERFORM 1000-I-INICIO   THRU                                 
                   1000-F-INICIO.                                       
                                                                        
           PERFORM 2000-I-PROCESO  THRU                                 
                   2000-F-PROCESO        UNTIL WS-FIN-PROCESO.          
                                                                        
           PERFORM 9999-I-FINAL    THRU                                 
                   9999-F-FINAL.                                        
                                                                        
       F-MAIN-PROGRAM. GOBACK.                                          
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO INICIO APERTURA ARCHIVOS   *                            
      *                                    *                            
      **************************************                            
       1000-I-INICIO.                                                   
                                                                        
           SET WS-SI-PROCESO TO TRUE.                                   
           SET WS-SI TO TRUE.                                           
                                                                        
           EXEC SQL                                                     
             OPEN CURSOR1                                               
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT EQUAL ZEROS                                   
              MOVE SQLCODE TO WS-SQLCODE                                
              DISPLAY 'ERROR EN OPEN DE CURSOR: ' WS-SQLCODE            
              MOVE 9999 TO RETURN-CODE                                  
              SET WS-FIN-PROCESO TO TRUE                                
           END-IF.                                                      
                                                                        
       1000-F-INICIO.   EXIT.                                           
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO PRINCIPAL DEL PROGRAMA     *                            
      *                                    *                            
      **************************************                            
       2000-I-PROCESO.                                                  
                                                                        
           EXEC SQL                                                     
              FETCH CURSOR1 INTO                                        
                  :DB-CU-TIPCUEN,                                       
                  :DB-CU-NROCUEN,                                       
                  :DB-CU-SUCUEN,                                        
                  :DB-CL-NOMAPE,                                        
                  :DB-CL-TIPDOC,                                        
                  :DB-CL-NRODOC                                         
           END-EXEC.                                                    
                                                                        
           EVALUATE TRUE                                                
             WHEN SQLCODE EQUAL ZEROS                                   
                MOVE DB-CL-TIPDOC TO WS-TIPDOC                          
                MOVE DB-CL-NRODOC TO WS-NRODOC                          
                PERFORM 3000-UPDATE-CUEN THRU 3000-F-UPDATE-CUEN        
             WHEN SQLCODE EQUAL +100                                    
                PERFORM 4000-UPDATE-CLI THRU 4000-F-UPDATE-CLI          
                SET WS-FIN-PROCESO TO TRUE                              
             WHEN OTHER                                                 
                MOVE SQLCODE TO WS-SQLCODE                              
                DISPLAY 'ERROR FETCH CURSOR: '   WS-SQLCODE             
                SET WS-NO TO TRUE                                       
            END-EVALUATE.                                               
                                                                        
       2000-F-PROCESO. EXIT.                                            
                                                                        
       3000-UPDATE-CUEN.                                                
                                                                        
            EXEC SQL                                                    
               UPDATE ITPLZRY.TB99CUEN                                  
                 SET NROCLI = 99                                        
                 WHERE TIPCUEN = :DB-CU-TIPCUEN                         
                 AND NROCUEN = :DB-CU-NROCUEN                           
            END-EXEC.                                                   
                                                                        
            IF SQLCODE NOT EQUAL ZEROS                                  
                MOVE SQLCODE TO WS-SQLCODE                              
                DISPLAY 'ERROR EN UPDATE CUENTA = ' WS-SQLCODE          
                SET WS-FIN-PROCESO TO TRUE                              
                SET WS-NO TO TRUE                                       
                MOVE 9999 TO RETURN-CODE                                
            END-IF.                                                     
       3000-F-UPDATE-CUEN. EXIT.                                        
                                                                        
       4000-UPDATE-CLI.                                                 
                                                                        
            EXEC SQL                                                    
               UPDATE ITPLZRY.TB99CLIE                                  
                 SET NROCLI = 99                                        
                 WHERE TIPDOC = :WS-TIPDOC AND                          
                       NRODOC = :WS-NRODOC                              
            END-EXEC.                                                   
                                                                        
            IF SQLCODE NOT EQUAL ZEROS                                  
                MOVE SQLCODE TO WS-SQLCODE                              
                DISPLAY 'ERROR EN UPDATE CLIENTE = ' WS-SQLCODE         
                SET WS-FIN-PROCESO TO TRUE                              
                SET WS-NO TO TRUE                                       
                MOVE 9999 TO RETURN-CODE                                
            ELSE                                                        
                SET WS-FIN-PROCESO TO TRUE                              
            END-IF.                                                     
                                                                        
       4000-F-UPDATE-CLI. EXIT.                                         
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO FINAL CIERRE DE FILES      *                            
      *                                    *                            
      **************************************                            
       9999-I-FINAL.                                                    
                                                                        
           EXEC SQL                                                     
              CLOSE CURSOR1                                             
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT EQUAL ZEROS                                   
              MOVE SQLCODE TO WS-SQLCODE                                
              DISPLAY '* ERROR CLOSE CURSOR      = ' WS-SQLCODE         
              MOVE 9999 TO RETURN-CODE                                  
           END-IF.                                                      
                                                                        
           IF WS-NO                                                     
              DISPLAY 'ROLLBACK REALIZADO'                              
              EXEC SQL                                                  
                  ROLLBACK                                              
              END-EXEC                                                  
                                                                        
           ELSE                                                         
              DISPLAY 'ACTUALIZACIONES CORRECTAS'                       
              DISPLAY 'COMMIT REALIZADO'                                
                                                                        
              EXEC SQL                                                  
                  COMMIT                                                
              END-EXEC                                                  
                                                                        
           END-IF.                                                      
                                                                        
      *    EXEC SQL                                                     
      *        ROLLBACK                                                 
      *    END-EXEC.                                                    
                                                                        
       9999-F-FINAL.                                                    
           EXIT.                                                        
      *                                                                 
