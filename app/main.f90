program main
  use lecturaJson
  use lista_clientes
  use Arbol_Capas
  use Arbol_Imagenes
  implicit none
  Type(listaClientes) :: milistaClientes
  Type(Cliente) :: clienteTemp
  type(ArbolCapas) :: miArbolCapas
  type(ArbolImagenes) :: miArbolImg
  integer :: opcion, opcionAdmin, opcionClient
  character(len=20) :: newNombre, newDPI, newPassword, loginNombre, loginPass

  do
    print *, "---------Menu Principal---------"
    print *, "1. Inicio de Sesion"
    print *, "2. Registro de Usuarios"
    print *, "3. Salida"
    read (*,*) opcion
    select case (opcion)
      case(1)
        print*, "Ingresa nombre de usuario:"
        read (*,'(A)') loginNombre
        print*, "Ingresa la contrasena:"
        read (*,*) loginPass
        clienteTemp = milistaClientes%buscarCliente(loginNombre,loginPass)

        if (loginNombre == "admin" .and. loginPass == "EDD2024") then
          !INTERFAZ DE ADMINISTRADOR
          do
            print *, "---------Modulo Administrador---------"
            print *, "1. Arbol B de usuarios"
            print *, "2. Operaciones sobre los usuarios"
            print *, "3. Carga Masiva de Usuarios"
            print *, "4. Regresar"
            read (*,*) opcionAdmin
            select case(opcionAdmin)
              case(1)
      
              case(2)
      
              case(3)
                call leerClientes(milistaClientes, &
                "C:\Users\Cesar\Documents\Programas\2024\EDD_PROYECTOF2_202202906\Clientes.json")
                print *, "Se han agregado los clientes correctamente"
              case(4)
                exit
              case default
                print *, "Selecciona algun valor que este en el menu"
            end select
          end do

        else if (clienteTemp%nombre /= "0") then
          ! INTERFAZ DE CLIENTE
          do
            print *, "---------Modulo Cliente---------"
            print *, "1. Reportes de las estructuras"
            print *, "2. Navegacion y gestion de imagenes"
            print *, "3. Carga Masiva de archivos"
            print *, "4. Regresar"
            read (*,*) opcionClient
            select case(opcionClient)
              case(1)
                call miArbolCapas%graficarABB()
                call miArbolImg%graficarAVL()
              case(2)
      
              case(3)
                call leerCapas(miArbolCapas, &
                "C:\Users\Cesar\Documents\Programas\2024\EDD_PROYECTOF2_202202906\Capas.json")
                print *,""
                print *,"ARBOL DE CAPAS HA SIDO LLENADO"
                call miArbolCapas%imprimirEnOrden()
                print *,""
                call leerImagenes(miArbolImg, miArbolCapas, &
                "C:\Users\Cesar\Documents\Programas\2024\EDD_PROYECTOF2_202202906\Imagenes.json")
                print *, "EL ARBOL DE IMAGENES HA SIDO COMPLETADO"
                call miArbolImg%imprimirArbolImg()
                print *, "\nCORROBORANDO ARBOL CAPAS ORIGINLA: "
                call miArbolCapas%imprimirEnOrden()
              case(4)
                exit
              case default
                print *, "Selecciona algun valor que este en el menu"
            end select
          end do

        else 
          print *,"Usuario/Contrasena erronea"
        end if
      case(2)
        print*, "Ingresa el nombre del nuevo usuario:"
        read (*,*) newNombre 
        clienteTemp%nombre = newNombre
        print*, "Ingresa el DPI del nuevo usuario:"
        read (*,*) newDPI
        clienteTemp%dpi = newDPI
        print*, "Ingresa la contrasena del nuevo usuario:"
        read (*,*) newPassword
        clienteTemp%password = newPassword

        call milistaClientes%agregarCliente(clienteTemp)
        call milistaClientes%printlista()
      case (3)
        exit
      case default
        print *, "Selecciona algun valor que este en el menu"
    end select
  end do
end program main
