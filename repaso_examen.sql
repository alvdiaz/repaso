CREATE OR REPLACE PACKAGE pkg_evaluacion IS
  v_error_rutina VARCHAR(350);
  v_error_mensaje VARCHAR(350);
  v_run_numero NUMBER;
  v_total_puntaje NUMBER;
  
  FUNCTION calcular_puntaje_zona_extrema(pa_zona_extrema NUMBER) RETURN NUMBER;
  FUNCTION calcular_puntaje_rank_institucion(pa_rank_institucion NUMBER) RETURN NUMBER;
END;
/

CREATE OR REPLACE PROCEDURE guardar_error(pa_run_numero NUMBER, pa_rutina_error VARCHAR2, pa_mensaje_error VARCHAR2) IS
BEGIN
  INSERT INTO ERROR_PROCESO VALUES(pa_run_numero, pa_rutina_error, pa_mensaje_error);
END;
/

CREATE OR REPLACE PACKAGE BODY pkg_evaluacion IS
  FUNCTION calcular_puntaje_zona_extrema(pa_zona_extrema NUMBER) RETURN NUMBER IS
  BEGIN
    SELECT PTJE_ZONA INTO pkg_evaluacion.v_total_puntaje
    FROM PTJE_ZONA_EXTREMA WHERE ZONA_EXTREMA = pa_zona_extrema;
    RETURN pkg_evaluacion.v_total_puntaje;
  END;

  FUNCTION calcular_puntaje_rank_institucion(pa_rank_institucion NUMBER) RETURN NUMBER IS
  BEGIN
    SELECT PTJE_RANKING INTO pkg_evaluacion.v_total_puntaje
    FROM PTJE_RANKING_INST
    WHERE pa_rank_institucion BETWEEN RANGO_RANKING_INI AND RANGO_RANKING_TER;
    RETURN pkg_evaluacion.v_total_puntaje;
  END; 
END;
/

CREATE OR REPLACE FUNCTION calcular_puntaje_horas_trabajo(pa_horas_trabajo NUMBER) RETURN NUMBER IS
BEGIN
  SELECT PTJE_HORAS_TRAB INTO pkg_evaluacion.v_total_puntaje 
  FROM PTJE_HORAS_TRABAJO 
  WHERE pa_horas_trabajo BETWEEN RANGO_HORAS_INI AND RANGO_HORAS_TER;    
  RETURN pkg_evaluacion.v_total_puntaje;
EXCEPTION  
  WHEN OTHERS THEN
    pkg_evaluacion.v_error_rutina := 'Error en la FUNCION_CALCULAR_PTJE_HORAS_TRABAJO al obtener puntaje con horas de trabajo semanal: ' || pa_horas_trabajo;
    pkg_evaluacion.v_error_mensaje := SQLERRM;
    guardar_error(pkg_evaluacion.v_run_numero, pkg_evaluacion.v_error_rutina, pkg_evaluacion.v_error_mensaje);
    RETURN 0;
END;
/

CREATE OR REPLACE FUNCTION calcular_puntaje_experiencia(pa_annos_experiencia NUMBER) RETURN NUMBER IS
BEGIN
  SELECT PTJE_EXPERIENCIA INTO pkg_evaluacion.v_total_puntaje 
  FROM PTJE_ANNOS_EXPERIENCIA 
  WHERE pa_annos_experiencia BETWEEN RANGO_ANNOS_INI AND RANGO_ANNOS_TER;
  RETURN pkg_evaluacion.v_total_puntaje;
EXCEPTION 
  WHEN OTHERS THEN
    pkg_evaluacion.v_error_rutina := 'Error en la FUNCION_CALCULAR_PTJE_EXPERIENCIA al obtener puntaje con años de experiencia: ' || pa_annos_experiencia;
    pkg_evaluacion.v_error_mensaje := SQLERRM;
    guardar_error(pkg_evaluacion.v_run_numero, pkg_evaluacion.v_error_rutina, pkg_evaluacion.v_error_mensaje);
    RETURN 0;
END;
/

CREATE OR REPLACE PROCEDURE procesar_postulaciones(pa_fecha VARCHAR2, pa_porcentaje_extra1 NUMBER, pa_porcentaje_extra2 NUMBER) IS  
  v_puntaje_experiencia NUMBER;
  v_puntaje_horas_trabajo NUMBER;
  v_puntaje_zona_extrema NUMBER;
  v_puntaje_rank_institucion NUMBER;
  v_puntaje_extra1 NUMBER;
  v_puntaje_extra2 NUMBER;
  v_suma_puntaje NUMBER;

  CURSOR cur_postulaciones IS
    SELECT A.NUMRUN, 
           ROUND(MONTHS_BETWEEN(pa_fecha, FECHA_NACIMIENTO) / 12) AS EDAD, 
           TO_CHAR(A.NUMRUN, '09G999G999') || '-' || DVRUN AS RUN_POSTULANTE, 
           UPPER(PNOMBRE) || ' ' || UPPER(SNOMBRE) || ' ' || UPPER(APATERNO) || ' ' || UPPER(AMATERNO) AS NOMBRE_POSTULANTE, 
           RANKING, 
           MAX(ROUND(MONTHS_BETWEEN(pa_fecha, FECHA_CONTRATO) / 12)) AS ANNOS_CONTRATO, 
           ROUND(SUM(HORAS_SEMANALES)) AS HORAS_SEMANALES, 
           NVL(ZONA_EXTREMA, 0) AS ZONA_EXTREMA
    FROM ANTECEDENTES_PERSONALES A 
    JOIN POSTULACION_PROGRAMA_ESPEC P ON A.NUMRUN = P.NUMRUN 
    JOIN PROGRAMA_ESPECIALIZACION PR ON P.COD_PROGRAMA = PR.COD_PROGRAMA 
    JOIN INSTITUCION I ON PR.COD_INST = I.COD_INST 
    JOIN ANTECEDENTES_LABORALES AN ON AN.NUMRUN = A.NUMRUN
    JOIN SERVICIO_SALUD S ON AN.COD_SERV_SALUD = S.COD_SERV_SALUD
    GROUP BY A.NUMRUN, 
             ROUND(MONTHS_BETWEEN(pa_fecha, FECHA_NACIMIENTO) / 12), 
             TO_CHAR(A.NUMRUN, '09G999G999') || '-' || DVRUN, 
             UPPER(PNOMBRE) || ' ' || UPPER(SNOMBRE) || ' ' || UPPER(APATERNO) || ' ' || UPPER(AMATERNO), 
             RANKING, 
             NVL(ZONA_EXTREMA, 0)
    ORDER BY A.NUMRUN;

BEGIN
  EXECUTE IMMEDIATE 'TRUNCATE TABLE DETALLE_PUNTAJE_POSTULACION';
  EXECUTE IMMEDIATE 'TRUNCATE TABLE ERROR_PROCESO';
  EXECUTE IMMEDIATE 'TRUNCATE TABLE RESULTADO_POSTULACION';
  
  FOR postulacion IN cur_postulaciones LOOP
    pkg_evaluacion.v_run_numero := postulacion.NUMRUN;
    
    IF postulacion.ZONA_EXTREMA >= 1 THEN
      v_puntaje_zona_extrema := pkg_evaluacion.calcular_puntaje_zona_extrema(postulacion.ZONA_EXTREMA);
    ELSE
      v_puntaje_zona_extrema := 0;
    END IF;
    
    v_puntaje_experiencia := calcular_puntaje_experiencia(postulacion.ANNOS_CONTRATO);
    v_puntaje_horas_trabajo := calcular_puntaje_horas_trabajo(postulacion.HORAS_SEMANALES);
    v_puntaje_rank_institucion := pkg_evaluacion.calcular_puntaje_rank_institucion(postulacion.RANKING);
    
    v_suma_puntaje := ROUND(v_puntaje_experiencia + v_puntaje_horas_trabajo + v_puntaje_zona_extrema + v_puntaje_rank_institucion);
    
    IF postulacion.EDAD <= 44 AND postulacion.HORAS_SEMANALES >= 31 THEN
      v_puntaje_extra1 := ROUND(v_suma_puntaje * (pa_porcentaje_extra1 / 100));
    ELSE
      v_puntaje_extra1 := 0;
    END IF;
    
    IF postulacion.ANNOS_CONTRATO > 25 THEN
      v_puntaje_extra2 := ROUND(v_suma_puntaje * (pa_porcentaje_extra2 / 100));
    ELSE
      v_puntaje_extra2 := 0;
    END IF;
    
    INSERT INTO DETALLE_PUNTAJE_POSTULACION VALUES(
      postulacion.RUN_POSTULANTE, 
      postulacion.NOMBRE_POSTULANTE, 
      v_puntaje_experiencia, 
      v_puntaje_horas_trabajo, 
      v_puntaje_zona_extrema, 
      v_puntaje_rank_institucion, 
      v_puntaje_extra1, 
      v_puntaje_extra2
    );
  END LOOP;
END;
/

CREATE OR REPLACE TRIGGER trg_postulacion
AFTER INSERT ON DETALLE_PUNTAJE_POSTULACION
FOR EACH ROW
DECLARE
  v_resultado VARCHAR2(15);
  v_suma_puntaje NUMBER;
BEGIN
  v_suma_puntaje := ROUND(:NEW.PTJE_ANNOS_EXP + :NEW.PTJE_HORAS_TRAB + 
                           :NEW.PTJE_ZONA_EXTREMA + :NEW.PTJE_RANKING_INST + 
                           :NEW.PTJE_EXTRA_1 + :NEW.PTJE_EXTRA_2);
  
  IF v_suma_puntaje >= 4500 THEN
    v_resultado := 'SELECCIONADO';
  ELSE
    v_resultado := 'NO SELECCIONADO';
  END IF;

  INSERT INTO RESULTADO_POSTULACION VALUES (:NEW.RUN_POSTULANTE, v_suma_puntaje, v_resultado);
END;
/

EXEC procesar_postulaciones('30/06/2024', 30, 15);

SELECT * FROM DETALLE_PUNTAJE_POSTULACION;
SELECT * FROM ERROR_PROCESO;
SELECT * FROM RESULTADO_POSTULACION;
