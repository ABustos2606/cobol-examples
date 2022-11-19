       IDENTIFICATION DIVISION.                                         
        PROGRAM-ID PGMVAZ15.                                            
                                                                        
      ***********************************************                   
      *                                             *                   
      *       CHECK-POINT 16 BATCH IMPRESION        *                   
      *               TP 30 - 24-10-2022            *                   
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
                                                                        
             SELECT SALIDA ASSIGN DDSALID                               
                    ORGANIZATION IS INDEXED                             
                    ACCESS MODE IS DYNAMIC                              
                    RECORD KEY IS KEY-SAL                               
                    FILE STATUS IS FS-SAL.                              
                                                                        
             SELECT LISTADO   ASSIGN DDLISTA                            
                    FILE STATUS IS FS-LIS.                              
                                                                        
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD ENTRADA                                                       
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
                                                                        
       01 REG-ENTRADA    PIC X(242).                                    
                                                                        
       FD SALIDA.                                                       
                                                                        
       01 REG-SALIDA.                                                   
          03 KEY-SAL     PIC X(17).                                     
          03 FILLER      PIC X(227).                                    
                                                                        
       FD LISTADO                                                       
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
                                                                        
       01 REG-LISTADO    PIC  X(132).                                   
                                                                        
                                                                        
       WORKING-STORAGE SECTION.                                         
      **************************************                            
                                                                        
       77  FS-ENT          PIC XX     VALUE SPACES.                     
       77  FS-SAL          PIC XX     VALUE SPACES.                     
       77  FS-LIS          PIC XX     VALUE SPACES.                     
                                                                        
       01  WS-STATUS-FIN   PIC X.                                       
           88  WS-FIN-LECTURA         VALUE 'Y'.                        
           88  WS-NO-FIN-LECTURA      VALUE 'N'.                        
                                                                        
       01  WS-VALIDADOR    PIC X.                                       
           88  WS-ERROR               VALUE 'Y'.                        
           88  WS-NO-ERROR            VALUE 'N'.                        
                                                                        
       77  WS-CONT-CLIENTE PIC 99     VALUE ZEROS.                      
                                                                        
       01  WS-FECHA-ACTUAL.                                             
           03 WS-ANIO               PIC 9(4)     VALUE ZEROS.           
           03 FILLER                PIC X(1)     VALUE '/'.             
           03 WS-MES                PIC 9(2)     VALUE ZEROS.           
           03 FILLER                PIC X(1)     VALUE '/'.             
           03 WS-DIA                PIC 9(2)     VALUE ZEROS.           
                                                                        
       01 WS-STATUS-FECHA           PIC X.                              
          88 WS-FECHA-VALIDA                     VALUE 'Y'.             
          88 WS-FECHA-NOT-VALIDA                 VALUE 'N'.             
                                                                        
       77  WS-RESULTADO             PIC 9(04)    VALUE ZEROS.           
       77  WS-RESTO-4               PIC 9(04)V99 VALUE ZEROS.           
       77  WS-RESTO-100             PIC 9(02)V99 VALUE ZEROS.           
       77  WS-RESTO-400             PIC 9(02)V99 VALUE ZEROS.           
                                                                        
           COPY TBCLIENT.                                               
           COPY TBVCLIEN.                                               
                                                                        
       77  WS-NRO-SECCION  PIC 99     VALUE ZEROS.                      
                                                                        
       77  WS-TOT-LEI      PIC 99     VALUE ZEROS.                      
       77  WS-TOT-ERR      PIC 99     VALUE ZEROS.                      
       77  WS-TOT-VAL      PIC 99     VALUE ZEROS.                      
       77  WS-TOT-ERR-F    PIC 999    VALUE ZEROS.                      
       77  WS-PRINT        PIC ZZ9    VALUE ZEROS.                      
                                                                        
       01  WS-REG-LISTADO.                                              
           03  FILLER         PIC X(17)    VALUE 'TIPO DE NOVEDAD: '.   
           03  WS-LIS-NOV     PIC XX       VALUE ZEROS.                 
           03  FILLER         PIC X(9)     VALUE SPACES.                
           03  FILLER         PIC X(19)    VALUE 'TIPO DE DOCUMENTO: '. 
           03  WS-LIS-TIP-DOC PIC XX       VALUE SPACES.                
           03  FILLER         PIC X(5)     VALUE SPACES.                
           03  FILLER         PIC X(18)    VALUE 'NRO DE DOCUMENTO: '.  
           03  WS-LIS-NRO-DOC PIC 9(11)    VALUE ZEROS.                 
           03  FILLER         PIC X        VALUE SPACES.                
                                                                        
       01  WS-REG-ERROR.                                                
           03  FILLER         PIC X(3)     VALUE SPACES.                
           03  FILLER         PIC X(26)                                 
                               VALUE 'ERROR ENCONTRADO EN CAMPO '.      
           03  WS-ERROR-TIPO  PIC X(30)    VALUE ZEROS.                 
           03  FILLER         PIC X(2)     VALUE ': '.                  
           03  WS-ERROR-CAMPO PIC X(30)    VALUE SPACES.                
           03  FILLER         PIC X        VALUE SPACES.                
                                                                        
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
                                                                        
       01  WS-SEPARADOR       PIC X(132)   VALUE ALL '*'.               
       01  WS-SEPARADOR-REG   PIC X(132)   VALUE ALL '='.               
                                                                        
       01  WS-REG-VALIDO.                                               
           03  FILLER         PIC X(3)     VALUE SPACES.                
           03  FILLER         PIC X(129)   VALUE 'REGISTRO VALIDO'.     
                                                                        
       01  WS-FECHA.                                                    
           03  WS-FECHA-AA    PIC 99       VALUE ZEROS.                 
           03  WS-FECHA-MM    PIC 99       VALUE ZEROS.                 
           03  WS-FECHA-DD    PIC 99       VALUE ZEROS.                 
                                                                        
       77  WS-CUENTA-LINEA    PIC 9(02)    VALUE ZEROS.                 
       77  WS-CUENTA-PAGINA   PIC 9(02)    VALUE 01.                    
       77  WS-CUENTA-ERROR    PIC 9(02)    VALUE ZEROS.                 
                                                                        
                                                                        
       PROCEDURE DIVISION.                                              
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
                                                                        
           OPEN I-O SALIDA.                                             
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
                                                                        
           PERFORM 5000-IMPRIMIR-REGISTRO THRU 5000-F-IMPRIMIR-REGISTRO.
           PERFORM 3000-VALIDACION THRU 3000-F-VALIDACION.              
           IF WS-ERROR THEN                                             
              ADD 1 TO WS-TOT-ERR                                       
           ELSE                                                         
              ADD 1 TO WS-TOT-VAL                                       
              PERFORM 5200-IMPRIMIR-VALIDO THRU 5200-F-IMPRIMIR-VALIDO  
              PERFORM 4000-GRABAR THRU 4000-F-GRABAR                    
           END-IF.                                                      
                                                                        
           PERFORM 5300-IMPRIMIR-SEPARADOR                              
                                      THRU 5300-F-IMPRIMIR-SEPARADOR    
           PERFORM 2100-LECTURA THRU 2100-F-LECTURA.                    
                                                                        
                                                                        
       2000-F-PROCESO. EXIT.                                            
                                                                        
      ***** LECTURA ARCHIVO NOVEDADES ******                            
       2100-LECTURA.                                                    
      **************************************                            
                                                                        
           READ ENTRADA INTO WN-TBCLIE.                                 
           EVALUATE FS-ENT                                              
             WHEN '00'                                                  
              ADD 1 TO WS-TOT-LEI                                       
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
                                                                        
           IF WN-CLI-TIPO-NOVEDAD IS NOT EQUAL TO 'AL'                  
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'NOVEDAD' TO WS-ERROR-TIPO                           
              MOVE WN-CLI-TIPO-NOVEDAD TO WS-ERROR-CAMPO                
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WN-CLI-TIPO-DOCUMENTO IS EQUAL TO                         
                             'DU' OR 'PA' OR 'LE' OR 'LC'               
              CONTINUE                                                  
           ELSE                                                         
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'TIPO DE DOCUMENTO' TO WS-ERROR-TIPO                 
              MOVE WN-CLI-TIPO-DOCUMENTO TO WS-ERROR-CAMPO              
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WN-CLI-NRO-DOCUMENTO IS NOT NUMERIC OR                    
              WN-CLI-NRO-DOCUMENTO < 1                                  
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'NRO DE DOCUMENTO' TO WS-ERROR-TIPO                  
              MOVE WN-CLI-NRO-DOCUMENTO TO WS-ERROR-CAMPO               
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WN-CLI-NRO-CLIENTE IS NOT NUMERIC OR                      
              WN-CLI-NRO-CLIENTE < 1                                    
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'NUMERO DE CLIENTE' TO WS-ERROR-TIPO                 
              MOVE WN-CLI-NRO-CLIENTE TO WS-ERROR-CAMPO                 
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WN-CLI-NOMBRE-CLIENTE IS EQUAL TO SPACES                  
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'NOMBRE' TO WS-ERROR-TIPO                            
              MOVE WN-CLI-NOMBRE-CLIENTE TO WS-ERROR-CAMPO              
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WN-CLI-APELLIDO-CLIENTE IS EQUAL TO SPACES                
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'APELLIDO' TO WS-ERROR-TIPO                          
              MOVE WN-CLI-APELLIDO-CLIENTE TO WS-ERROR-CAMPO            
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WN-CLI-DOMICILIO IS EQUAL TO SPACES                       
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'DOMICILIO' TO WS-ERROR-TIPO                         
              MOVE WN-CLI-DOMICILIO TO WS-ERROR-CAMPO                   
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WN-CLI-CIUDAD IS EQUAL TO SPACES                          
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'CIUDAD' TO WS-ERROR-TIPO                            
              MOVE WN-CLI-CIUDAD TO WS-ERROR-CAMPO                      
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WN-CLI-CODIGO-POSTAL IS EQUAL TO SPACES                   
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'CODIGO POSTAL' TO WS-ERROR-TIPO                     
              MOVE WN-CLI-CODIGO-POSTAL TO WS-ERROR-CAMPO               
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WN-CLI-NACIONALIDAD IS EQUAL TO SPACES                    
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'NACIONALIDAD' TO WS-ERROR-TIPO                      
              MOVE WN-CLI-NACIONALIDAD TO WS-ERROR-CAMPO                
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WN-CLI-FECHA-DE-ALTA IS EQUAL TO SPACES                   
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'FECHA DE ALTA' TO WS-ERROR-TIPO                     
              MOVE WN-CLI-FECHA-DE-ALTA TO WS-ERROR-CAMPO               
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WN-CLI-FECHA-DE-BAJA IS EQUAL TO SPACES                   
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'FECHA DE BAJA' TO WS-ERROR-TIPO                     
              MOVE WN-CLI-FECHA-DE-BAJA TO WS-ERROR-CAMPO               
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WN-CLI-ESTADO-CIVIL IS EQUAL TO                           
                             'SO' OR 'VI' OR 'CA' OR 'DI' OR 'UC'       
              CONTINUE                                                  
           ELSE                                                         
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'ESTADO CIVIL' TO WS-ERROR-TIPO                      
              MOVE WN-CLI-ESTADO-CIVIL TO WS-ERROR-CAMPO                
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WN-CLI-SEXO IS EQUAL TO 'F ' OR 'M ' OR 'O '              
              CONTINUE                                                  
           ELSE                                                         
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'SEXO' TO WS-ERROR-TIPO                              
              MOVE WN-CLI-SEXO TO WS-ERROR-CAMPO                        
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           IF WN-CLI-CORREO-ELECTRONICO IS EQUAL TO SPACES              
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'CORREO ELECTRONICO' TO WS-ERROR-TIPO                
              MOVE WN-CLI-CORREO-ELECTRONICO TO WS-ERROR-CAMPO          
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
           MOVE WN-CLI-FECCHA-NACIMIENTO TO WS-FECHA-ACTUAL.            
                                                                        
           PERFORM 3100-VALIDAR-FECHA                                   
                   THRU 3100-F-VALIDAR-FECHA.                           
                                                                        
           IF WS-FECHA-NOT-VALIDA THEN                                  
              SET WS-ERROR TO TRUE                                      
              INITIALIZE WS-REG-ERROR                                   
              MOVE 'FECHA DE NACIMIENTO' TO WS-ERROR-TIPO               
              MOVE WN-CLI-FECCHA-NACIMIENTO TO WS-ERROR-CAMPO           
              PERFORM 5100-IMPRIMIR-ERROR THRU 5100-F-IMPRIMIR-ERROR    
           END-IF.                                                      
                                                                        
       3000-F-VALIDACION. EXIT.                                         
                                                                        
                                                                        
      ***** VALIDACION DE FECHA ************                            
       3100-VALIDAR-FECHA.                                              
      **************************************                            
                                                                        
           SET WS-FECHA-VALIDA TO TRUE.                                 
                                                                        
           IF WS-ANIO IS NOT NUMERIC OR                                 
              WS-MES  IS NOT NUMERIC OR                                 
              WS-DIA  IS NOT NUMERIC                                    
                 SET WS-FECHA-NOT-VALIDA TO TRUE                        
           END-IF.                                                      
                                                                        
           IF WS-FECHA-VALIDA                                           
                 IF WS-ANIO < 1922 OR WS-ANIO > 2003                    
                    SET WS-FECHA-NOT-VALIDA TO TRUE                     
                 END-IF                                                 
                                                                        
                 IF WS-MES < 00 OR WS-MES > 13                          
                    SET WS-FECHA-NOT-VALIDA TO TRUE                     
                 END-IF                                                 
                                                                        
                 IF WS-MES = 02                                         
                  IF WS-DIA > 28                                        
                     IF WS-DIA > 29                                     
                        SET WS-FECHA-NOT-VALIDA TO TRUE                 
                     ELSE                                               
                       DIVIDE WS-ANIO BY 004 GIVING WS-RESULTADO        
                                      REMAINDER WS-RESTO-4              
                       DIVIDE WS-ANIO BY 100 GIVING WS-RESULTADO        
                                      REMAINDER WS-RESTO-100            
                       DIVIDE WS-ANIO BY 400 GIVING WS-RESULTADO        
                                      REMAINDER WS-RESTO-400            
                       IF NOT ((WS-RESTO-4 EQUAL 0 AND                  
                              WS-RESTO-100 NOT EQUAL 0) OR              
                              WS-RESTO-400 EQUAL 0)                     
                                 SET WS-FECHA-NOT-VALIDA TO TRUE        
                     END-IF                                             
                  END-IF                                                
                 END-IF                                                 
                                                                        
                 IF WS-MES IS EQUAL TO (4 OR 6 OR 9 OR 11) AND          
                  WS-DIA > 30                                           
                     SET WS-FECHA-NOT-VALIDA TO TRUE                    
                 END-IF                                                 
                 IF WS-MES IS EQUAL TO                                  
                   (1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12) AND              
                   WS-DIA > 31                                          
                     SET WS-FECHA-NOT-VALIDA TO TRUE                    
                 END-IF                                                 
                                                                        
           END-IF.                                                      
                                                                        
       3100-F-VALIDAR-FECHA. EXIT.                                      
                                                                        
                                                                        
                                                                        
      ***** GRABACION ARCHIVO SALIDA *******                            
       4000-GRABAR.                                                     
      **************************************                            
                                                                        
           ADD 1 TO WS-CONT-CLIENTE                                     
                                                                        
           INITIALIZE WK-TBCLIE                                         
                                                                        
           MOVE WN-CLI-TIPO-NOVEDAD       TO WK-CLI-TIPO-NOVEDAD        
           MOVE WN-CLI-TIPO-DOCUMENTO     TO WK-CLI-TIPO-DOCUMENTO      
           MOVE WN-CLI-NRO-DOCUMENTO      TO WK-CLI-NRO-DOCUMENTO       
           MOVE WS-CONT-CLIENTE           TO WK-CLI-NRO-SEC             
           MOVE WN-CLI-NRO-CLIENTE        TO WK-CLI-NRO-CLIENTE         
           MOVE WN-CLI-NOMBRE-CLIENTE     TO WK-CLI-NOMBRE-CLIENTE      
           MOVE WN-CLI-APELLIDO-CLIENTE   TO WK-CLI-APELLIDO-CLIENTE    
           MOVE WN-CLI-DOMICILIO          TO WK-CLI-DOMICILIO           
           MOVE WN-CLI-CIUDAD             TO WK-CLI-CIUDAD              
           MOVE WN-CLI-CODIGO-POSTAL      TO WK-CLI-CODIGO-POSTAL       
           MOVE WN-CLI-NACIONALIDAD       TO WK-CLI-NACIONALIDAD        
           MOVE WN-CLI-FECHA-DE-ALTA      TO WK-CLI-FECHA-DE-ALTA       
           MOVE WN-CLI-FECHA-DE-BAJA      TO WK-CLI-FECHA-DE-BAJA       
           MOVE WN-CLI-ESTADO-CIVIL       TO WK-CLI-ESTADO-CIVIL        
           MOVE WN-CLI-SEXO               TO WK-CLI-SEXO                
           MOVE WN-CLI-CORREO-ELECTRONICO TO WK-CLI-CORREO-ELECTRONICO  
           MOVE WN-CLI-FECCHA-NACIMIENTO  TO WK-CLI-FECCHA-NACIMIENTO.  
                                                                        
           WRITE REG-SALIDA   FROM WK-TBCLIE                            
                                                                        
           IF FS-SAL EQUAL ZEROS                                        
                CONTINUE                                                
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
                                                                        
           INITIALIZE WS-REG-LISTADO.                                   
           MOVE WN-CLI-TIPO-NOVEDAD   TO WS-LIS-NOV.                    
           MOVE WN-CLI-TIPO-DOCUMENTO TO WS-LIS-TIP-DOC.                
           MOVE WN-CLI-NRO-DOCUMENTO  TO WS-LIS-NRO-DOC.                
                                                                        
           WRITE REG-LISTADO  FROM WS-REG-LISTADO AFTER 1.              
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
             ADD 1 TO WS-TOT-ERR-F.                                     
                                                                        
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
           MOVE 3 TO WS-CUENTA-LINEA.                                   
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
                                                                        
       5500-F-IMPRIMIR-TITULOS. EXIT.                                   
                                                                        
       9999-I-FINAL.                                                    
                                                                        
           MOVE WS-TOT-LEI TO WS-PRINT                                  
           DISPLAY "NOVEDADES LEIDAS = " WS-PRINT                       
           MOVE WS-TOT-VAL TO WS-PRINT                                  
           DISPLAY "REGISTROS VALIDADOS = " WS-PRINT                    
           MOVE WS-TOT-ERR TO WS-PRINT                                  
           DISPLAY "REGISTROS CON ERROR = " WS-PRINT                    
           MOVE  WS-TOT-ERR-F TO WS-PRINT                               
           DISPLAY "ERRORES ENCONTRADOS = " WS-PRINT                    
                                                                        
           CLOSE ENTRADA                                                
              IF FS-ENT IS NOT EQUAL '00'                               
                DISPLAY '* ERROR EN CLOSE ENTRADA = '                   
                                            FS-ENT                      
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