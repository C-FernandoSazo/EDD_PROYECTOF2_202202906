program main
  use lecturaJson
  use Arbol_clientes
  use matriz_DispersaI
  use Arbol_CapaBssss
  use Arbol_Imagenes
  use listaAlbums
  implicit none
  Type(ArbolClientes) :: miArbolClientes
  Type(Cliente) :: clienteTemp, newCliente
  type(matrizDispersa) :: matriztmp
  type(ArbolCapas) :: miArbolCapas
  type(ArbolImagenes) :: miArbolImg
  type(lista_album) :: miListaAlbum
  type(NodoImagen), pointer :: imagenTemp, imagenSearch
  type(Capa) :: capaTemp
  character(len=5) :: opCaracter
  integer :: opcion, opcionAdmin, opcionClient, opcionReportClient, opcionGenImg, limite, opcionLimit
  integer :: contadorLimite, opcionOPClient, clientModificar, profundidad, amplitud, idcapa, idImg, graph
  integer(8) ::  newDPI
  character(len=20) :: newNombre, newPassword, loginNombre, loginPass
  character(len=100) :: rutaArchivo

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
            print *, "4. Cerrar Sesion"
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
                      newCliente%nombre = newNombre
                      print*, "Ingresa el DPI del nuevo usuario:"
                      read (*,*) newDPI
                      newCliente%dpi = newDPI
                      print*, "Ingresa la contrasena del nuevo usuario:"
                      read (*,*) newPassword
                      newCliente%password = newPassword
              
                      call miArbolClientes%insert(newCliente)
                    case(2)
                      print *,"Selecciona el ID del cliente que deseas modificar"
                      call miArbolClientes%imprimirClientes()
                      read (*,*) clientModificar
                      print*, "Ingresa el nuevo nombre del usuario:"
                      read (*,*) newNombre 
                      print*, "Ingresa la nueva contrasena del usuario:"
                      read (*,*) newPassword
                      call miArbolClientes%modificarCliente(clientModificar,newNombre,newPassword)
                    case(3)
                      exit
                  end select 
                end do
              case(3)
                print *,"Ingresa la ruta del archivo de Clientes"
                read(*,*) rutaArchivo
                call leerClientes(miArbolClientes,rutaArchivo)
                print *, "Se han agregado los clientes correctamente"
              case(4)
                exit
              case default
                print *, "Selecciona algun valor que este en el menu"
            end select
          end do

        else if (clienteTemp%nombre /= "0") then
          ! INTERFAZ DE CLIENTE
          miArbolImg = clienteTemp%miArbolImg
          miListaAlbum = clienteTemp%miListaAlbum
          miArbolCapas = clienteTemp%miArbolCapas
          do
            print *, "---------Modulo Cliente---------"
            print *, "1. Estado de las estructuras"
            print *, "2. Navegacion y gestion de imagenes"
            print *, "3. Carga Masiva de archivos"
            print *, "4. Reportes de Usuario"
            print *, "5. Cerrar Sesion"
            read (*,*) opcionClient
            select case(opcionClient)
              case(1)
                do
                  print *, "---------Estructuras---------"
                  print *, "1. Ver Arbol de Imagenes"
                  print *, "2. Ver Arbol de Capas"
                  print *, "3. Ver Listado de Albumes"
                  print *, "4. Ver Capa"
                  print *, "5. Ver Imagen y Arbol de Capas"
                  print *, "6. Regresar"
                  read(*,*) graph
                  select case(graph)
                    case(1)
                      call miArbolImg%graficarAVL()
                    case(2)
                      call miArbolCapas%graficarABB()
                    case(3)
                      call miListaAlbum%graficar_albums()
                    case(4)
                      print*,"Capas disponibles:"
                      call miArbolCapas%imprimirEnOrden()
                      print*,"Selecciona el id de una capa"
                      read(*,*) idcapa
                      print *,idcapa
                      capaTemp = miArbolCapas%buscarNodo(idcapa)
                      call capaTemp%matriz%graficarMatrizDispersa()
                    case(5)
                      print *, "Imagenes disponibles: "
                      call miArbolImg%imprimirArbolImg()
                      print *, "Selecciona el id de la imagen que deseas visualizar su arbol de capas"
                      read(*,*) idImg
                      print *,idImg
                      call miArbolImg%graficarAVL_BB(idImg)
                    case(6)
                      exit
                    case default
                      print *,"Selecciona una opcion valida"
                  end select
                end do
              case(2)
                do
                  print *,"Gestion de imagenes"
                  print *,"1. Insertar nueva imagen"
                  print *,"2. Eliminar imagen"
                  print *,"3. Regresar"
                  read (*,*) opcionGenImg
                  select case(opcionGenImg)
                  case(1)
                    do
                      call matriztmp%vaciarMatriz()
                      print *, "Como te gustaria generar una nueva imagen"
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
                          call matriztmp%generarImagen()
                        case(3)
                          print *,"Con que id deseas guardar tu imagen (solo valores numericos)"
                          read(*,*) idImg
                          imagenSearch => miArbolImg%buscarImg(idImg)
                          if (.not. associated(imagenSearch)) then
                            call miArbolImg%insertar(idImg)
                            do
                              print*,"Capas disponibles:"
                              call miArbolCapas%imprimirEnOrden()
                              print*,"Selecciona el id de una capa"
                              read(*,*) idcapa
                              capaTemp = miArbolCapas%buscarNodo(idcapa)
                              call miArbolCapas%apilarMatriz(idcapa,matriztmp)
                              call miArbolImg%ingresarCapas(idImg,capaTemp%key,capaTemp%matriz)
                              print *,"Deseas ingresar otra capa? (si/no)"
                              read (*,*) opCaracter
                              select case(opCaracter)
                              case('si')
                                continue
                              case('no')
                                print *,"Imagen guardada con exito"
                                call matriztmp%generarImagen()
                                exit
                              end select
                            end do
                          else 
                            print *,"El id ingresado ya existe, selecciona otro"
                          end if
                        case(4)
                          exit
                        case default
                          print *, "Selecciona una opcion valida"
                        end select
                    end do
                  case(2)
                    print *,"Selecciona el id de la imagen que deseas eliminar"
                    call miArbolImg%imprimirArbolImg()
                    read (*,*) idImg
                    call miArbolImg%eliminar(idImg)
                    call miListaAlbum%deleteImagen(idImg)
                  case(3)
                    exit
                  end select
                end do
              case(3)
                print *,"Ingresa la ruta del archivo de Capas"
                read(*,*) rutaArchivo
                call leerCapas(miArbolCapas,rutaArchivo)
                print *,"Ingresa la ruta del archivo de Imagenes"
                read(*,*) rutaArchivo
                call leerImagenes(miArbolImg, miArbolCapas, rutaArchivo)
                print *,"Ingresa la ruta del archivo de Albumes"
                read(*,*) rutaArchivo
                call leerAlbumes(miListaAlbum, rutaArchivo)
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
                      write(*,'(A,I0)') "La profundidad del arbol es: ",profundidad
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
                clienteTemp%miArbolCapas = miArbolCapas
                clienteTemp%miArbolImg = miArbolImg
                clienteTemp%miListaAlbum = miListaAlbum
                call miArbolClientes%actualizarEstructuras(clienteTemp%dpi,clienteTemp)
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
        newCliente%nombre = newNombre
        print*, "Ingresa el DPI del nuevo usuario:"
        read (*,*) newDPI
        newCliente%dpi = newDPI
        print*, "Ingresa la contrasena del nuevo usuario:"
        read (*,*) newPassword
        newCliente%password = newPassword

        call miArbolClientes%insert(newCliente)
      case (3)
        exit
      case default
        print *, "Selecciona algun valor que este en el menu"
    end select
  end do
end program main
