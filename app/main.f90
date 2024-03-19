program main
  use lecturaJson
  use Arbol_clientes
  use matriz_DispersaI
  use Arbol_CapaBss
  use Arbol_Imagenes
  use listaAlbum
  implicit none
  Type(ArbolClientes) :: miArbolClientes
  Type(Cliente) :: clienteTemp
  type(matrizDispersa) :: matriztmp
  type(ArbolCapas) :: miArbolCapas
  type(ArbolImagenes) :: miArbolImg
  type(lista_album) :: miListaAlbum
  type(NodoImagen), pointer :: imagenTemp
  character(len=5) :: opCaracter
  integer :: opcion, opcionAdmin, opcionClient, opcionReportClient, opcionGenImg, limite, opcionLimit
  integer :: contadorLimite, opcionOPClient, clientModificar, profundidad, amplitud, idcapa
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
        clienteTemp = miArbolClientes%buscar(loginNombre,loginPass)

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
                do
                  print *, "1. Insertar Usuario"
                  print *, "2. Modificar Usuario"
                  print *, "3. Regresar"
                  read(*,*) opcionOPClient
                  select case (opcionOPClient)
                    case(1)
                      print*, "Ingresa el nombre del nuevo usuario:"
                      read (*,*) newNombre 
                      clienteTemp%nombre = newNombre
                      print*, "Ingresa el DPI del nuevo usuario:"
                      read (*,*) newDPI
                      clienteTemp%dpi = newDPI
                      print*, "Ingresa la contrasena del nuevo usuario:"
                      read (*,*) newPassword
                      clienteTemp%password = newPassword
              
                      call miArbolClientes%insert(clienteTemp)
                    case(2)
                      print *,"Selecciona el ID del cliente que deseas modificar"
                      call miArbolClientes%imprimirClientes()
                      read (*,*) clientModificar
                      print*, "Ingresa el nuevo nombre del usuario:"
                      read (*,*) newNombre 
                      print*, "Ingresa el nuevo DPI del usuario:"
                      read (*,*) newDPI
                      print*, "Ingresa la nueva contrasena del usuario:"
                      read (*,*) newPassword
                      call miArbolClientes%modificarCliente(clientModificar,newNombre,newPassword,newDPI)
                  end select 
                end do
              case(3)
                call leerClientes(miArbolClientes, &
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
            print *, "1. Estado de las estructuras"
            print *, "2. Navegacion y gestion de imagenes"
            print *, "3. Carga Masiva de archivos"
            print *, "4. Reportes de Usuario"
            print *, "5. Regresar"
            read (*,*) opcionClient
            select case(opcionClient)
              case(1)
                call miArbolCapas%graficarABB()
                call miArbolImg%graficarAVL()
                call miListaAlbum%graficar_albums()
              case(2)
                do
                  call matriztmp%vaciarMatriz()
                  print *, "Como te gustaria agregar una nueva imagen"
                  print *, "1. Por recorrido limitado"
                  print *, "2. Por arbol de imagenes"
                  print *, "3. Por capa"
                  print *, "4. Regresar"
                  read(*,*) opcionGenImg
                  select case(opcionGenImg)
                    case(1)
                      print *, "Escoge el recorrido: "
                      print *, "1. Preorder"
                      print *, "2. Inorder"
                      print *, "3. Postorder"
                      read (*,*) opcionLimit
                      print *, "Ingresa el limite del recorrido:"
                      read (*,*) limite
                      contadorLimite = 0
                      select case(opcionLimit)
                        case(1)
                          call miArbolCapas%preorderLimit(miArbolCapas%raiz,limite,contadorLimite,matriztmp)
                        case(2)
                          call miArbolCapas%inorderLimit(miArbolCapas%raiz,limite,contadorLimite,matriztmp)
                        case(3)
                          call miArbolCapas%postorderLimit(miArbolCapas%raiz,limite,contadorLimite,matriztmp)
                        case default
                          print *, "Selecciona una opcion valida"
                      end select
                      call matriztmp%graficarMatrizDispersa()
                      call matriztmp%generarImagen()
                    case(2)
                      print *, "Imagenes disponibles: "
                      call miArbolImg%imprimirArbolImg()
                      print *, "Selecciona el id de la imagen que deseas visualizar"
                      read(*,*) amplitud
                      imagenTemp => miArbolImg%buscarImg(amplitud)
                      print *, "Arbol encontrado"
                      call imagenTemp%arbolCapa%graficarABB()
                      print *, "RECORRIDO EN AMPLITUD"
                      call imagenTemp%arbolCapa%recorridoAmplitud(matriztmp)
                      call matriztmp%graficarMatrizDispersa()
                      call matriztmp%generarImagen()
                    case(3)
                      do
                        print*,"Capas disponibles:"
                        call miArbolCapas%imprimirEnOrden()
                        print*,"Selecciona el id de una"
                        read(*,*) idcapa
                        call miArbolCapas%apilarMatriz(idcapa,matriztmp)
                        print *,"Deseas ingresar otra capa? (si/no)"
                        read (*,*) opCaracter
                        select case(opCaracter)
                        case('si')
                          continue
                        case('no')
                          call matriztmp%graficarMatrizDispersa()
                          call matriztmp%generarImagen()
                          exit
                        end select
                      end do
                    case(4)
                      exit
                    case default
                      print *, "Selecciona una opcion valida"
                    end select
                end do
              case(3)
                call leerCapas(miArbolCapas, &
                "C:\Users\Cesar\Documents\Programas\2024\EDD_PROYECTOF2_202202906\Capas.json")
                call leerImagenes(miArbolImg, miArbolCapas, &
                "C:\Users\Cesar\Documents\Programas\2024\EDD_PROYECTOF2_202202906\Imagenes.json")
                call leerAlbumes(miListaAlbum,"C:\Users\Cesar\Documents\Programas\2024\EDD_PROYECTOF2_202202906\Albumes.json")
              case(4)
                do
                  print *, "1. Top 5 de imagenes con mas numero de capas"
                  print *, "2. Todas las capas que son hojas"
                  print *, "3. Profundidad de arbol de capas"
                  print *, "4. Listar las capas en: preorden, inorden, postorden"
                  print *, "5. Regresar"
                  read (*,*) opcionReportClient
                  select case(opcionReportClient)
                    case(1)
                    case(2)
                      call miArbolCapas%imprimirHojas()
                    case(3)
                      profundidad =  miArbolCapas%calcularProfundidad()
                      write(*,'(A,I0)') "Profundidad del arbol: ",profundidad
                    case(4)
                      print *, "Recorrido Preorder"
                      call miArbolCapas%preorder(miArbolCapas%raiz)
                      print *,""
                      print *, "Recorrido Inorder"
                      call miArbolCapas%inorder(miArbolCapas%raiz)
                      print *,""
                      print *, "Recorrido Postorder"
                      call miArbolCapas%postorder(miArbolCapas%raiz)
                      print *,""
                    case(5)
                      exit
                  end select
                end do
              case(5)
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

        call miArbolClientes%insert(clienteTemp)
      case (3)
        exit
      case default
        print *, "Selecciona algun valor que este en el menu"
    end select
  end do
end program main
