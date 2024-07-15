CREATE OR REPLACE PACKAGE evaluacion_pkg IS
  error_rutina VARCHAR2(350);
  error_mensaje VARCHAR2(350);
  run_id NUMBER;
  total_puntaje NUMBER;
  
  FUNCTION puntaje_zona_extrema(zona_extrema_valor NUMBER) RETURN NUMBER;
  FUNCTION puntaje_ranking_institucion(ranking_institucion_valor NUMBER) RETURN NUMBER;
END;
/

CREATE OR REPLACE PROCEDURE registrar_error(run_id_valor NUMBER, rutina_error VARCHAR2, mensaje_error VARCHAR2) IS
BEGIN
  INSERT INTO ERROR_PROCESO VALUES(run_id_valor, rutina_error, mensaje_error);
END;
/

CREATE OR REPLACE PACKAGE BODY evaluacion_pkg IS
  FUNCTION puntaje_zona_extrema(zona_extrema_valor NUMBER) RETURN NUMBER IS
  BEGIN
    SELECT PTJE_ZONA INTO evaluacion_pkg.total_puntaje
    FROM PTJE_ZONA_EXTREMA WHERE ZONA_EXTREMA = zona_extrema_valor;
    RETURN evaluacion_pkg.total_puntaje;
  END;

  FUNCTION puntaje_ranking_institucion(ranking_institucion_valor NUMBER) RETURN NUMBER IS
  BEGIN
    SELECT PTJE_RANKING INTO evaluacion_pkg.total_puntaje
    FROM PTJE_RANKING_INST
    WHERE ranking_institucion_valor BETWEEN RANGO_RANKING_INI AND RANGO_RANKING_TER;
    RETURN evaluacion_pkg.total_puntaje;
  END; 
END;
/

CREATE OR REPLACE FUNCTION calcular_puntaje_horas_trabajo(horas_trabajo_valor NUMBER) RETURN NUMBER IS
BEGIN
  SELECT PTJE_HORAS_TRAB INTO evaluacion_pkg.total_puntaje 
  FROM PTJE_HORAS_TRABAJO 
  WHERE horas_trabajo_valor BETWEEN RANGO_HORAS_INI AND RANGO_HORAS_TER;    
  RETURN evaluacion_pkg.total_puntaje;
EXCEPTION  
  WHEN OTHERS THEN
    evaluacion_pkg.error_rutina := 'Error en FUNCION_CALCULAR_PTJE_HORAS_TRABAJO al obtener puntaje con horas de trabajo semanal: ' || horas_trabajo_valor;
    evaluacion_pkg.error_mensaje := SQLERRM;
    registrar_error(evaluacion_pkg.run_id, evaluacion_pkg.error_rutina, evaluacion_pkg.error_mensaje);
    RETURN 0;
END;
/

CREATE OR REPLACE FUNCTION calcular_puntaje_experiencia(experiencia_valor NUMBER) RETURN NUMBER IS
BEGIN
  SELECT PTJE_EXPERIENCIA INTO evaluacion_pkg.total_puntaje 
  FROM PTJE_ANNOS_EXPERIENCIA 
  WHERE experiencia_valor BETWEEN RANGO_ANNOS_INI AND RANGO_ANNOS_TER;
  RETURN evaluacion_pkg.total_puntaje;
EXCEPTION 
  WHEN OTHERS THEN
    evaluacion_pkg.error_rutina := 'Error en FUNCION_CALCULAR_PTJE_EXPERIENCIA al obtener puntaje con años de experiencia: ' || experiencia_valor;
    evaluacion_pkg.error_mensaje := SQLERRM;
    registrar_error(evaluacion_pkg.run_id, evaluacion_pkg.error_rutina, evaluacion_pkg.error_mensaje);
    RETURN 0;
END;
/

CREATE OR REPLACE PROCEDURE procesar_postulaciones(fecha_valor VARCHAR2, porcentaje_extra1 NUMBER, porcentaje_extra2 NUMBER) IS  
  puntaje_experiencia NUMBER;
  puntaje_horas_trabajo NUMBER;
  puntaje_zona_extrema NUMBER;
  puntaje_ranking_institucion NUMBER;
  puntaje_extra1 NUMBER;
  puntaje_extra2 NUMBER;
  suma_total_puntaje NUMBER;

  CURSOR cursor_postulaciones IS
    SELECT A.NUMRUN, 
           ROUND(MONTHS_BETWEEN(fecha_valor, FECHA_NACIMIENTO) / 12) AS EDAD, 
           TO_CHAR(A.NUMRUN, '09G999G999') || '-' || DVRUN AS RUN_POSTULANTE, 
           UPPER(PNOMBRE) || ' ' || UPPER(SNOMBRE) || ' ' || UPPER(APATERNO) || ' ' || UPPER(AMATERNO) AS NOMBRE_POSTULANTE, 
           RANKING, 
           MAX(ROUND(MONTHS_BETWEEN(fecha_valor, FECHA_CONTRATO) / 12)) AS ANNOS_CONTRATO, 
           ROUND(SUM(HORAS_SEMANALES)) AS HORAS_SEMANALES, 
           NVL(ZONA_EXTREMA, 0) AS ZONA_EXTREMA
    FROM ANTECEDENTES_PERSONALES A 
    JOIN POSTULACION_PROGRAMA_ESPEC P ON A.NUMRUN = P.NUMRUN 
    JOIN PROGRAMA_ESPECIALIZACION PR ON P.COD_PROGRAMA = PR.COD_PROGRAMA 
    JOIN INSTITUCION I ON PR.COD_INST = I.COD_INST 
    JOIN ANTECEDENTES_LABORALES AN ON AN.NUMRUN = A.NUMRUN
    JOIN SERVICIO_SALUD S ON AN.COD_SERV_SALUD = S.COD_SERV_SALUD
    GROUP BY A.NUMRUN, 
             ROUND(MONTHS_BETWEEN(fecha_valor, FECHA_NACIMIENTO) / 12), 
             TO_CHAR(A.NUMRUN, '09G999G999') || '-' || DVRUN, 
             UPPER(PNOMBRE) || ' ' || UPPER(SNOMBRE) || ' ' || UPPER(APATERNO) || ' ' || UPPER(AMATERNO), 
             RANKING, 
             NVL(ZONA_EXTREMA, 0)
    ORDER BY A.NUMRUN;

BEGIN
  EXECUTE IMMEDIATE 'TRUNCATE TABLE DETALLE_PUNTAJE_POSTULACION';
  EXECUTE IMMEDIATE 'TRUNCATE TABLE ERROR_PROCESO';
  EXECUTE IMMEDIATE 'TRUNCATE TABLE RESULTADO_POSTULACION';
  
  FOR postulacion IN cursor_postulaciones LOOP
    evaluacion_pkg.run_id := postulacion.NUMRUN;
    
    IF postulacion.ZONA_EXTREMA >= 1 THEN
      puntaje_zona_extrema := evaluacion_pkg.puntaje_zona_extrema(postulacion.ZONA_EXTREMA);
    ELSE
      puntaje_zona_extrema := 0;
    END IF;
    
    puntaje_experiencia := calcular_puntaje_experiencia(postulacion.ANNOS_CONTRATO);
    puntaje_horas_trabajo := calcular_puntaje_horas_trabajo(postulacion.HORAS_SEMANALES);
    puntaje_ranking_institucion := evaluacion_pkg.puntaje_ranking_institucion(postulacion.RANKING);
    
    suma_total_puntaje := ROUND(puntaje_experiencia + puntaje_horas_trabajo + puntaje_zona_extrema + puntaje_ranking_institucion);
    
    IF postulacion.EDAD <= 44 AND postulacion.HORAS_SEMANALES >= 31 THEN
      puntaje_extra1 := ROUND(suma_total_puntaje * (porcentaje_extra1 / 100));
    ELSE
      puntaje_extra1 := 0;
    END IF;
    
    IF postulacion.ANNOS_CONTRATO > 25 THEN
      puntaje_extra2 := ROUND(suma_total_puntaje * (porcentaje_extra2 / 100));
    ELSE
      puntaje_extra2 := 0;
    END IF;
    
    INSERT INTO DETALLE_PUNTAJE_POSTULACION VALUES(
      postulacion.RUN_POSTULANTE, 
      postulacion.NOMBRE_POSTULANTE, 
      puntaje_experiencia, 
      puntaje_horas_trabajo, 
      puntaje_zona_extrema, 
      puntaje_ranking_institucion, 
      puntaje_extra1, 
      puntaje_extra2
    );
  END LOOP;
END;
/

CREATE OR REPLACE TRIGGER trigger_postulacion
AFTER INSERT ON DETALLE_PUNTAJE_POSTULACION
FOR EACH ROW
DECLARE
  resultado VARCHAR2(15);
  suma_total_puntaje NUMBER;
BEGIN
  suma_total_puntaje := ROUND(:NEW.PTJE_ANNOS_EXP + :NEW.PTJE_HORAS_TRAB + 
                              :NEW.PTJE_ZONA_EXTREMA + :NEW.PTJE_RANKING_INST + 
                              :NEW.PTJE_EXTRA_1 + :NEW.PTJE_EXTRA_2);
  
  IF suma_total_puntaje >= 4500 THEN
    resultado := 'SELECCIONADO';
  ELSE
    resultado := 'NO SELECCIONADO';
  END IF;

  INSERT INTO RESULTADO_POSTULACION VALUES (:NEW.RUN_POSTULANTE, suma_total_puntaje, resultado);
END;
/

EXEC procesar_postulaciones('30/06/2024', 30, 15);

SELECT * FROM DETALLE_PUNTAJE_POSTULACION;
SELECT * FROM ERROR_PROCESO;
SELECT * FROM RESULTADO_POSTULACION;
