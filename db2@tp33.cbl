       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID PGMDB215.                                             
      **********************************************************        
      *                                                        *        
      *              PROGRAMA PARA SQL EMBEBIDO                *        
      *         CHECK-POINT 28 BATCH ACT DB2 - TP 33           *        
      *                       3-11-22                          *        
      *                                                        *        
      **********************************************************        
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
               SELECT ENTRADA ASSIGN DDENTRA                            
               ORGANIZATION IS INDEXED                                  
               ACCESS MODE IS SEQUENTIAL                                
               RECORD KEY IS K-NOV                                      
               FILE STATUS IS FS-ENT.                                   
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD ENTRADA.                                                      
                                                                        
       01 REG-ENTRADA.                                                  
          03  K-NOV.                                                    
              05  K-TIPNOV    PIC X(2).                                 
              05  K-TIPDOC    PIC X(2).                                 
              05  K-NRODOC    PIC X(11).                                
              05  K-NROSEC    PIC X(2).                                 
          03  FILLER          PIC X(227).                               
                                                                        
      **************************************                            
       WORKING-STORAGE SECTION.                                         
      **************************************                            
       77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.  
                                                                        
       77  FS-ENT           PIC XX    VALUE SPACES.                     
       01  WS-FLAG-FIN      PIC X.                                      
           88  WS-SI-PROCESO      VALUE ' '.                            
           88  WS-FIN-PROCESO     VALUE 'F'.                            
                                                                        
       77  FILLER        PIC X(26) VALUE '* VARIABLES SQL          *'.  
       77  WS-SQLCODE    PIC +++999 USAGE DISPLAY VALUE ZEROS.          
                                                                        
            EXEC SQL                                                    
              INCLUDE SQLCA                                             
            END-EXEC.                                                   
                                                                        
       01  WS-REG-CLIENTE.                                              
           03 WS-CLI-TIPDOC     PIC XX         VALUE SPACES.            
           03 WS-CLI-NRODOC     PIC S9(11)V USAGE COMP-3 VALUE ZEROS.   
           03 WS-CLI-NROCLI     PIC S9(03)V USAGE COMP-3 VALUE ZEROS.   
           03 WS-CLI-NOMAPE     PIC X(30)      VALUE SPACES.            
           03 WS-CLI-NOMBRE     PIC X(15)      VALUE SPACES.            
           03 WS-CLI-APELLIDO   PIC X(15)      VALUE SPACES.            
           03 WS-CLI-FECNAC     PIC X(10)      VALUE SPACES.            
           03 WS-CLI-SEXO       PIC X          VALUE SPACES.            
                                                                        
       COPY  TBVCLIEN.                                                  
                                                                        
       01  WS-TOT-LEIDAS        PIC 99   VALUE ZEROS.                   
       01  WS-TOT-INSERT        PIC 99   VALUE ZEROS.                   
       01  WS-TOT-ERROR         PIC 99   VALUE ZEROS.                   
       01  WS-CONTADOR          PIC 9    VALUE 1.                       
                                                                        
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
                                                                        
           OPEN INPUT ENTRADA.                                          
           IF FS-ENT IS NOT EQUAL '00'                                  
              DISPLAY '* ERROR EN OPEN ENTRADA = ' FS-ENT               
              MOVE 9999 TO RETURN-CODE                                  
              SET  WS-FIN-PROCESO TO TRUE                               
           END-IF.                                                      
                                                                        
       1000-F-INICIO.   EXIT.                                           
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO PRINCIPAL DEL PROGRAMA     *                            
      *                                    *                            
      **************************************                            
       2000-I-PROCESO.                                                  
                                                                        
           READ ENTRADA INTO WK-TBCLIE                                  
           EVALUATE FS-ENT                                              
              WHEN '00'                                                 
                 ADD 1 TO WS-TOT-LEIDAS                                 
                 PERFORM 3000-INSERT THRU 3000-F-INSERT                 
              WHEN '10'                                                 
                 SET WS-FIN-PROCESO TO TRUE                             
              WHEN OTHER                                                
                 DISPLAY 'ERROR EN LECTURA ENTRADA' FS-ENT              
                 MOVE 9999 TO RETURN-CODE                               
                 SET WS-FIN-PROCESO TO TRUE                             
           END-EVALUATE.                                                
                                                                        
           IF WS-CONTADOR EQUAL 3                                       
              SET WS-FIN-PROCESO TO TRUE                                
           END-IF.                                                      
                                                                        
           ADD 1 TO WS-CONTADOR.                                        
                                                                        
       2000-F-PROCESO. EXIT.                                            
                                                                        
                                                                        
       3000-INSERT.                                                     
                                                                        
           MOVE SPACES TO WS-REG-CLIENTE                                
           MOVE WK-CLI-TIPO-DOCUMENTO TO WS-CLI-TIPDOC                  
           MOVE WK-CLI-NRO-DOCUMENTO TO WS-CLI-NRODOC                   
           MOVE WK-CLI-NRO-CLIENTE TO WS-CLI-NROCLI                     
           MOVE WK-CLI-NOMBRE-CLIENTE TO WS-CLI-NOMBRE                  
           MOVE WK-CLI-APELLIDO-CLIENTE TO WS-CLI-APELLIDO              
           MOVE WK-CLI-FECCHA-NACIMIENTO TO WS-CLI-FECNAC               
           MOVE WK-CLI-SEXO TO WS-CLI-SEXO                              
                                                                        
           STRING WS-CLI-NOMBRE DELIMITED BY '  '                       
                  ' ' DELIMITED BY SIZE                                 
                  WS-CLI-APELLIDO DELIMITED BY '  '                     
                  INTO WS-CLI-NOMAPE                                    
           END-STRING.                                                  
                                                                        
           EXEC SQL                                                     
                INSERT INTO ITPLZRY.TB99CLIE                            
                      (TIPDOC,                                          
                       NRODOC,                                          
                       NROCLI,                                          
                       NOMAPE,                                          
                       FECNAC,                                          
                       SEXO)                                            
                 VALUES                                                 
                      (:WS-CLI-TIPDOC,                                  
                       :WS-CLI-NRODOC,                                  
                       :WS-CLI-NROCLI,                                  
                       :WS-CLI-NOMAPE,                                  
                       :WS-CLI-FECNAC,                                  
                       :WS-CLI-SEXO)                                    
           END-EXEC.                                                    
                                                                        
           EVALUATE SQLCODE                                             
             WHEN +0                                                    
                ADD 1 TO WS-TOT-INSERT                                  
             WHEN -803                                                  
                ADD 1 TO WS-TOT-ERROR                                   
                DISPLAY 'CLIENTE DUPLICADO'                             
             WHEN OTHER                                                 
                MOVE SQLCODE TO WS-SQLCODE                              
                DISPLAY 'ERROR ACCESO TABLA: ' WS-SQLCODE               
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-PROCESO TO TRUE                              
           END-EVALUATE.                                                
                                                                        
       3000-F-INSERT. EXIT.                                             
                                                                        
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO FINAL CIERRE DE FILES      *                            
      *                                    *                            
      **************************************                            
       9999-I-FINAL.                                                    
                                                                        
           DISPLAY 'NOVEDADES LEIDAS     = ' WS-TOT-LEIDAS              
           DISPLAY 'NOVEDADES INSERTADAS = ' WS-TOT-INSERT              
           DISPLAY 'NOVEDADES ERRONEAS   = ' WS-TOT-ERROR               
                                                                        
           CLOSE ENTRADA                                                
             IF FS-ENT       IS NOT EQUAL '00'                          
               DISPLAY '* ERROR EN CLOSE ENTRADA = '                    
                                           FS-ENT                       
               MOVE 9999 TO RETURN-CODE                                 
            END-IF.                                                     
                                                                        
      *    EXEC SQL                                                     
      *        ROLLBACK                                                 
      *    END-EXEC.                                                    
                                                                        
       9999-F-FINAL.                                                    
           EXIT.                                                        
      *                                                                 