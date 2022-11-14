       IDENTIFICATION DIVISION.                                         
      *------------------------*                                        
       PROGRAM-ID. CORPERF2.                                            
      ********************************************************          
      *                                                      *          
      *            CORTE DE CONTROL CON PERFORMS             *          
      *                                                      *          
      *                                                      *          
      ********************************************************          
       ENVIRONMENT DIVISION.                                            
      *---------------------*                                           
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT ENTRADA ASSIGN TO DDENTRA                             
           FILE STATUS IS FS-ENT.                                       
                                                                          
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                          
       FD  ENTRADA                                                      
           BLOCK CONTAINS  0 RECORDS                                    
           RECORDING MODE IS F.                                         
                                                                          
       01  REG-ENTRADA        PIC X(160).                               
                                                                          
       WORKING-STORAGE SECTION.                                         
      *------------------------*                                        
                                                                          
       01  FS-ENT             PIC X(02).                                
           88 FS-OK                      VALUE '00'.                    
           88 FS-EOF                     VALUE '10'.                    
           88 FS-NOK                     VALUE '01' THRU '09'           
                                               '11' THRU '99'.          
           COPY CPPERSO.                                                
                                                                          
      * VARIABLES *                                                     
       01  WS-LOC-ANT         PIC X(15).                                
       01  WS-SEXO-ANT        PIC X.                                    
                                                                          
      * ACUMULADORES *                                                  
                                                                          
      * CONTADORES *                                                    
       01  CN-TOT-GEN         PIC 9(03)   VALUE ZEROS.                  
       01  CN-TOT-SEXO        PIC 9(03)   VALUE ZEROS.                  
       01  CN-TOT-LOC         PIC 9(03)   VALUE ZEROS.                  
       01  CN-TOT-F           PIC 9(03)   VALUE ZEROS.                  
       01  CN-TOT-M           PIC 9(03)   VALUE ZEROS.                  
       01  CN-TOT-O           PIC 9(03)   VALUE ZEROS.                  
                                                                          
      * IMPRESION *                                                     
       01  PR-TOTALES         PIC ZZ9.                                  
                                                                          
       01  WS-SEPARADOR       PIC X(50)   VALUE ALL '-'.                
                                                                          
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
           OPEN INPUT ENTRADA                                           
                                                                         
           IF NOT FS-OK                                                 
              DISPLAY '* ERROR EN OPEN ENTRADA = ' FS-ENT               
              MOVE 9999 TO RETURN-CODE                                  
           END-IF                                                       
           .                                                            
       1000-F-INICIO.                                                   
           EXIT.                                                        
                                                                          
      **************************************                            
      *  CUERPO PRINCIPAL DE PROCESOS      *                            
      **************************************                            
       2000-I-PROCESO.                                                  
      *---------------*                                                 
           READ ENTRADA INTO REG-PERSONA                                
                                                                          
           PERFORM UNTIL FS-EOF                                         
                      OR FS-NOK                                         
                                                                          
               MOVE PER-LOCALIDAD TO WS-LOC-ANT                         
               MOVE 0 TO CN-TOT-LOC                                     
               DISPLAY 'LOCALIDAD: ' WS-LOC-ANT                         
                                                                          
      * CORTE A *                                                       
               PERFORM UNTIL FS-EOF                                     
                          OR FS-NOK                                     
                          OR PER-LOCALIDAD NOT = WS-LOC-ANT             
                                                                         
                   MOVE PER-SEXO TO WS-SEXO-ANT                         
                   MOVE 0 TO CN-TOT-SEXO                                
                                                                          
      * CORTE B *                                                       
                   PERFORM UNTIL FS-EOF                                 
                              OR FS-NOK                                 
                              OR PER-LOCALIDAD NOT = WS-LOC-ANT         
                              OR PER-SEXO NOT = WS-SEXO-ANT             
                                                                          
                       ADD 1 TO CN-TOT-SEXO                             
                       ADD 1 TO CN-TOT-GEN                              
                                                                        
                       READ ENTRADA INTO REG-PERSONA                    
                                                                          
                   END-PERFORM                                          
                                                                          
                   ADD  CN-TOT-SEXO TO CN-TOT-LOC                       
                   MOVE CN-TOT-SEXO TO PR-TOTALES                       
                   EVALUATE WS-SEXO-ANT                                 
                      WHEN 'F'                                          
                         DISPLAY '   FEMENINO  = ' PR-TOTALES           
                         ADD CN-TOT-SEXO TO CN-TOT-F                    
                      WHEN 'M'                                          
                         DISPLAY '   MASCULINO = ' PR-TOTALES           
                         ADD CN-TOT-SEXO TO CN-TOT-M                    
                      WHEN 'O'                                          
                         DISPLAY '   OTROS     = ' PR-TOTALES           
                         ADD CN-TOT-SEXO TO CN-TOT-O                    
                      WHEN OTHER                                        
                         DISPLAY '   ERRONEOS  = ' PR-TOTALES           
                   END-EVALUATE                                         
               END-PERFORM                                              

               MOVE CN-TOT-LOC TO PR-TOTALES                            
               DISPLAY ' '                                              
               DISPLAY 'TOTAL GENERAL ' WS-LOC-ANT ' = ' PR-TOTALES     
               DISPLAY WS-SEPARADOR                                     
 
           END-PERFORM                                                  


           MOVE CN-TOT-GEN TO PR-TOTALES                                
           DISPLAY 'TOTAL GENERAL DE PERSONAS = ' PR-TOTALES            
           MOVE CN-TOT-F TO PR-TOTALES                                  
           DISPLAY '   FEMENINO  = ' PR-TOTALES                         
           MOVE CN-TOT-M TO PR-TOTALES                                  
           DISPLAY '   MASCULINO = ' PR-TOTALES                         
           MOVE CN-TOT-O TO PR-TOTALES                                  
           DISPLAY '   OTROS     = ' PR-TOTALES                         
           DISPLAY WS-SEPARADOR                                         
           .                                                            
       2000-F-PROCESO.                                                  
           EXIT.                                                        
 

       9999-I-FINAL.                                                    
      *-------------*                                                   
           CLOSE ENTRADA                                                
           IF NOT FS-OK                                                 
              DISPLAY '* ERROR EN CLOSE ENTRADA = ' FS-ENT              
              MOVE 9999 TO RETURN-CODE                                  
           END-IF                                                       
           .                                                            
       9999-F-FINAL.                                                    
           EXIT.                                                        

