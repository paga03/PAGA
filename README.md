Funcion celda1 <- caja1(dato)
		ancho = 20 - Longitud(dato)
		espacio = " "
		Para j=1 Hasta ancho Con Paso 1 Hacer
			celda1 <- Concatenar(celda1,espacio)
		Fin Para
		celda1<-Concatenar(dato,celda1)
Fin Funcion
	
Funcion celda2 <- caja2(dato)
		ancho = 16 - Longitud(dato)
		espacio = " "
		Para j=1 Hasta ancho Con Paso 1 Hacer
			celda2 <- Concatenar(celda2,espacio)
		Fin Para
		celda2<-Concatenar(dato,celda2)
Fin Funcion
	
	Algoritmo Calculadora_para_pruebas_diagnosticas
		//Esta es una caculadora para indices de pruebas diagnosticas.
		Escribir "Buen dia señor usuario,"
		Escribir "Esta herramienta es una calculadora para la obtener los valores e índices, de una prueba para diagnostico clínico aplicada a un grupo de individuos."
		
		//Ahora se definen algunas variables auxiliares y los vetores y matrices necesarios.
		Definir enfermedad, prevprueba, sensibilidad, especificidad, VPPP, VPNP, VPPA, VPNA, CPP, CPN Como Caracter
		Definir a, b, c, d, e, f, g, h, n Como entero
		Definir prevalencia Como Real
		
		//Se solicitan los datos al usuario.
	Repetir
		Escribir "Para iniciar es necesario que usted ingrese algunos datos que se solicitan a continuación:"
		Escribir "Primero empecemos por digitar el nombre de la enfermedad."
		Leer enfermedad
		Escribir "Bien, ahora digitemos el valor del % de la prevalencia de la enfermedad en la poblacion (Si la desconoce, digite 0)."
		Leer prevalencia
		Escribir "Bien, ahora digitemos ¿Cantidad de personas enfermas que presentan un resultado positivo en la prueba diagnóstica?"
		Leer a
		Escribir "Bien, ahora digitemos ¿Cantidad de personas no enfermas que presentan un resultado positivo en la prueba diagnóstica?"
		Leer b
		Escribir "Bien, ahora digitemos ¿Cantidad de personas enfermas que presentan un resultado negativo en la prueba diagnóstica?"
		Leer c
		Escribir "Bien, ahora digitemos ¿Cantidad de personas no enfermas que presentan un resultado negativo en la prueba diagnóstica?"
		Leer d
		
		//Se ralizan todos lo calculos de INDICES necesarios para mostrar al usuario segun los valores ingresados.
		e = a+b
		f = c+d
		g = a+c
		h = b+d
		n = a+b+c+d
		prevalencia<-prevalencia/100
		prevprueba <- subcadena(ConvertirATexto(100*g/n),1,5)
		sensibilidad = subcadena(ConvertirATexto(100*a/g),1,5)
		especificidad = subcadena(ConvertirATexto(100*d/h),1,5)
		VPPP = subcadena(ConvertirATexto(100*a/e),1,5)
		VPNP = subcadena(ConvertirATexto(100*d/f),1,5)
		VPPA = subcadena(ConvertirATexto(100*([prevalencia*a/g]/[(prevalencia*a/g)+((1-prevalencia)*(1-(d/h)))])),1,5)
		VPNA = subcadena(ConvertirATexto(100*([(1-prevalencia)*d/h]/[((1-prevalencia)*d/h)+((1-(a/g))*prevalencia)])),1,5)
		
		
		//muestra al usuario la tabla de 2X2 PARA VERIFICAR VALORES	
		celda1<-caja1(enfermedad)
		celda2<-caja2(enfermedad)
		celda1 <- Mayusculas(celda1)
		celda2 <- Mayusculas(celda2)
		celdaa<-ConvertirATexto(a)
		celdaa<-caja1(celdaa)
		celdab<-ConvertirATexto(b)
		celdab<-caja1(celdab)
		celdac<-ConvertirATexto(c)
		celdac<-caja1(celdac)
		celdad<-ConvertirATexto(d)
		celdad<-caja1(celdad)
		celdae<-ConvertirATexto(e)
		celdae<-caja1(celdae)
		celdaf<-ConvertirATexto(f)
		celdaf<-caja1(celdaf)
		celdag<-ConvertirATexto(g)
		celdag<-caja1(celdag)
		celdah<-ConvertirATexto(h)
		celdah<-caja1(celdah)
		celdan<-ConvertirATexto(n)
		celdan<-caja1(celdan)
		
		
		Escribir "|-------------|--------------------|--------------------|--------------------|"
		Escribir "|PRUEBA\ESTADO|", celda1, "|SIN ", celda2, "|MARGINAL            |"
		Escribir "|-------------|--------------------|--------------------|--------------------|"
		Escribir "|      +      |", celdaa, "|", celdab, "|", celdae, "|"
		Escribir "|-------------|--------------------|--------------------|--------------------|"
		Escribir "|      -      |", celdac, "|", celdad, "|", celdaf, "|"
		Escribir "|-------------|--------------------|--------------------|--------------------|"
		Escribir "|MARGINAL     |", celdag, "|", celdah, "|", celdan, "|"
		Escribir "|-------------|--------------------|--------------------|--------------------|"
		
		Escribir " "
		Escribir "Verifique que los datos estan correctos en la tabla:"
		Escribir "Digite 1. Si la tabla es correcta, de lo contrario digite 2."
		leer inicio
		
		Mientras inicio <1 o inicio >2
			Escribir "Por favor verifique que los datos estan correctos en la tabla: Digite solamente 1.Si o 2.No."
			leer inicio
			Si inicio = 2 Entonces
				Escribir "**************"
				Escribir "Detecto errores en la tabla, volvamos a empezar."
				Escribir "**************"
			Fin Si
		Fin Mientras
		
	Hasta Que inicio = 1
	
		//muestra al usuario el menu de opcionmes de calculos que puede realizar y mostrar la calculadora hasta que decida salir
		i=0
		Mientras i<>8 Hacer
			Escribir "Por favor digite el numero del indice que desea calcular:"
			Escribir " "
			Escribir "1. Calculo de Prevalencia."
			Escribir "2. Calculo de la Sensibilidad de la prueba diagnostica."
			Escribir "3. Calculo de la Especificidad de la prueba diagnostica."
			Escribir "4. Calculo del Valor Predictivo Positivo de la prueba."
			Escribir "5. Calculo del Valor Predictivo Negativo de la prueba."
			Escribir "6. Calculo del Valor Predictivo Positivo ajuastado a la prevalencia de la poblacion."
			Escribir "7. Calculo del Valor Predictivo Negativo ajuastado a la prevalencia de la poblacion."
			Escribir "8. Salir."
			
			leer i
			Si i<1 o i>8 Entonces
				Escribir "Este numero no es una opcion disponible, por favor digite un numero de opcion valido entre 1 y 7"
				Escribir "****************************************"
				Escribir " "
			SiNo
				Segun i Hacer
					1:
						Escribir "****************************************"
						Escribir "La prevalencia de la poblacion es = (", prevalencia, "%), pero la prevalencia estimada de la prueba diagnostica es = (", prevprueba, "%): Esta es la probabilidad de que una persona este enferma." 
						Escribir "****************************************"
						Escribir " "
						
					2:
						Escribir "****************************************"
						Escribir "SENSIBILIDAD = (", sensibilidad, "%): Esta es la probabilidad de que una persona obtenga un resultado positivo en la prueba diagnostica, dado que esta enfermo." 
						Escribir "****************************************"
						Escribir " "
						
					3:
						Escribir "****************************************"
						Escribir "ESPECIFICIDAD = (", especificidad, "%): Esta es la probabilidad de que una persona obtenga un resultado negativo en la prueba diagnostica, dado que no esta enfermo."
						Escribir "****************************************"
						Escribir " "
					4:
						Escribir "****************************************"
						Escribir "VALOR PREDICTIVO POSITIVO DE LA PRUEBA VPPP= (", VPPP, "%): Esta es la probabilidad de que una persona este enferma, dado que obtuvo un resultado positivo en la prueba diagnostica."
						Escribir "****************************************"
						Escribir " "
					5:	
						Escribir "****************************************"
						Escribir "VALOR PREDICTIVO NEGATIVO DE LA PRUEBA VPNP= (", VPNP, "%): Esta es la probabilidad de que una persona no este enferma, dado que obtuvo un resultado negativo en la prueba diagnostica."
						Escribir "****************************************"
						Escribir " "	
					6:
						Escribir "****************************************"
						Si prevalencia = 0 Entonces
							Escribir "No se puede calcular el VALOR PREDICTIVO POSITIVO AJUSTADO (VPPA) por que la prevalencia de la poblacion es cero (0%)"
						SiNo
							Escribir "VALOR PREDICTIVO POSITIVO AJUSTADO VPPA= (", VPPA, "%): Esta es la probabilidad de que una persona este enferma, dado que obtuvo un resultado positivo en la prueba diagnostica, ajustado con la prevalencia poblacional."
						Fin Si
						Escribir "****************************************"
						Escribir " "	
					7:
						Escribir "****************************************"
						Si prevalencia = 0 Entonces
							Escribir "No se puede calcular el VALOR PREDICTIVO NEGATIVO AJUSTADO (VPNA) por que la prevalencia de la poblacion es cero (0%)"
						SiNo
						Escribir "VALOR PREDICTIVO NEGATIVO AJUSTADO VPNP= (", VPNA, "%): Esta es la probabilidad de que una persona no este enferma, dado que obtuvo un resultado negativo en la prueba diagnostica, ajustado con la prevalencia poblacional."
						Fin Si
						Escribir "****************************************"
						Escribir " "
						
					De Otro Modo:
						Escribir "GRACIAS POR HABER USADO LA CALCULADORA DE PRUEBAS DIAGNOSTICAS"
				Fin Segun
			Fin Si
		Fin Mientras
	
FinAlgoritmo
