module moduleAC
  implicit none

contains 

  subroutine moduloAdmin(milistaClientes)
    use lista_clientes
    use lecturaJson
    implicit none
    Type(listaClientes), intent(inout) :: milistaClientes
    integer :: opcion
    do
      print *, "---------Modulo Administrador---------"
      print *, "1. Arbol B de usuarios"
      print *, "2. Operaciones sobre los usuarios"
      print *, "3. Carga Masiva de Usuarios"
      print *, "4. Regresar"
      read (*,*) opcion
      select case(opcion)
        case(1)

        case(2)

        case(3)
          call leerClientes(milistaClientes,"C:\Users\Cesar\Documents\Programas\2024\EDD_PROYECTOF2_202202906\Clientes.json")
          print *, "Se han agregado los clientes correctamente"
        case(4)
          exit
        case default
          print *, "Selecciona algun valor que este en el menu"
      end select
    end do
  end subroutine moduloAdmin

  subroutine moduloClient(client, miArbolCapas)
    use lecturaJson
    use lista_clientes
    use Arbol_Capas
    implicit none
    type(Cliente), intent(inout) :: client
    type(ArbolCapas), intent(inout) :: miArbolCapas
    integer :: opcion
    do
      print *, "---------Modulo Cliente---------"
      print *, "1. Reportes de las estructuras"
      print *, "2. Navegacion y gestion de imagenes"
      print *, "3. Carga Masiva de archivos"
      print *, "4. Regresar"
      read (*,*) opcion
      select case(opcion)
        case(1)
          call miArbolCapas%graficarABB()
        case(2)

        case(3)
          call leerCapas(miArbolCapas,"C:\Users\Cesar\Documents\Programas\2024\EDD_PROYECTOF2_202202906\Capas.json")
          call miArbolCapas%imprimirEnOrden()
        case(4)
          exit
        case default
          print *, "Selecciona algun valor que este en el menu"
      end select
    end do
  end subroutine moduloClient

end module moduleAC

program main
  use moduleAC
  use lista_clientes
  use Arbol_Capas
  implicit none
  Type(listaClientes) :: milistaClientes
  Type(Cliente) :: clienteTemp
  type(ArbolCapas) :: miArbolCapas
  integer :: opcion
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
          call moduloAdmin(milistaClientes)
        else if (clienteTemp%nombre /= "0") then
          call moduloClient(clienteTemp,miArbolCapas)
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
