module matriz_dispersa
    implicit none

    type :: nodo_matriz
        integer :: fila
        integer :: columna
        character(len=7) :: color
        type(nodo_matriz), pointer :: siguiente => null()
    end type nodo_matriz

    type :: matrizDispersa
        integer :: rows
        integer :: cols
        type(nodo_matriz), pointer :: head => null()
        contains
            procedure :: add
            procedure :: vaciarMatrix
            procedure :: printMatriz
    end type matrizDispersa

contains

    subroutine add(self, fila, columna, color)
        class(matrizDispersa), intent(inout) :: self
        integer, intent(in) :: fila, columna
        character(len=7), intent(in) :: color
        type(nodo_matriz), pointer :: nuevoNodo, actual, anterior

        ! Crear y configurar el nuevo nodo
        allocate(nuevoNodo)
        nuevoNodo%fila = fila
        nuevoNodo%columna = columna
        nuevoNodo%color = color
        nuevoNodo%siguiente => null()

        ! Inserción en lista vacía
        if (.not. associated(self%head)) then
            self%head => nuevoNodo
            return
        end if

        actual => self%head
        anterior => null()

        ! Buscar la posición correcta para inserción
        do while(associated(actual))
            if (fila < actual%fila .or. (fila == actual%fila .and. columna < actual%columna)) then
                exit
            end if
            anterior => actual
            actual => actual%siguiente
        end do

        ! Inserción al inicio
        if (.not. associated(anterior)) then
            nuevoNodo%siguiente => self%head
            self%head => nuevoNodo
        else
            ! Inserción en medio o al final
            nuevoNodo%siguiente => actual
            anterior%siguiente => nuevoNodo
        end if
    end subroutine add

    subroutine vaciarMatrix(matriz)
        class(matrizDispersa), intent(inout) :: matriz
        type(nodo_matriz), pointer :: temp

        do while(associated(matriz%head))
            temp => matriz%head
            matriz%head => matriz%head%siguiente
            deallocate(temp)
        end do
    end subroutine vaciarMatrix

    subroutine printMatriz(matriz)
        class(matrizDispersa), intent(inout) :: matriz
        type(nodo_matriz), pointer :: actual

        actual => matriz%head
        do while(associated(actual)) 
            write(*,*) "Fila: ", actual%fila, " Columna: ", actual%columna, " Color: ", &
            trim(actual%color)
            actual => actual%siguiente
        end do
    end subroutine printMatriz

end module matriz_dispersa


module Arbol_Capas
    use matriz_dispersa
    implicit none
    ! Arbol Binario de Busqueda ABB

    type, public :: NodoCapa
        integer :: key
        type(matrizDispersa) :: matriz
        type(NodoCapa), pointer :: left => null()
        type(NodoCapa), pointer :: right => null()
    end type NodoCapa

    type, public :: ArbolCapas 
        type(NodoCapa), pointer :: raiz => null()
        contains
            procedure :: insertarNodo
            procedure :: imprimirEnOrden
            procedure :: graficarABB
    end type ArbolCapas

contains 

    subroutine insertarNodo(arbol, key, matriz)
        class(ArbolCapas), intent(inout) :: arbol
        integer, intent(in) :: key
        type(matrizDispersa) :: matriz
        type(NodoCapa), pointer :: nuevoNodo

        ! Crear un nuevo nodo
        allocate(nuevoNodo)
        nuevoNodo%key = key
        nuevoNodo%matriz = matriz
        nuevoNodo%left => null()
        nuevoNodo%right => null()

        if (.not. associated(arbol%raiz)) then
            ! Si el árbol está vacío, el nuevo nodo es la raíz
            arbol%raiz => nuevoNodo
        else
            ! De lo contrario, llamar a la subrutina recursiva para insertar el nodo
            call insertarRecursivo(arbol%raiz, nuevoNodo)
        end if
    end subroutine insertarNodo

    ! Subrutina recursiva para insertar el nodo en la posición correcta
    recursive subroutine insertarRecursivo(nodo, nuevoNodo)
        type(NodoCapa), pointer, intent(inout) :: nodo
        type(NodoCapa), pointer, intent(in) :: nuevoNodo

        if (nuevoNodo%key < nodo%key) then
            if (.not. associated(nodo%left)) then
                nodo%left => nuevoNodo
            else
                call insertarRecursivo(nodo%left, nuevoNodo)
            end if
        else if (nuevoNodo%key > nodo%key) then
            if (.not. associated(nodo%right)) then
                nodo%right => nuevoNodo
            else
                call insertarRecursivo(nodo%right, nuevoNodo)
            end if
        else
            print *, "La clave ya existe en el árbol."
        end if
    end subroutine insertarRecursivo

    subroutine imprimirEnOrden(arbol)
        class(ArbolCapas), intent(in) :: arbol
        ! Si el árbol no está vacío, imprimir recursivamente
        if (associated(arbol%raiz)) then
            call imprimirRecursivo(arbol%raiz)
        end if
    end subroutine imprimirEnOrden

    ! Subrutina recursiva para imprimir los nodos en orden
    recursive subroutine imprimirRecursivo(nodo)
        type(NodoCapa), pointer, intent(in) :: nodo
        
        if (associated(nodo)) then
            ! Imprimir subárbol izquierdo
            call imprimirRecursivo(nodo%left)
            ! Imprimir clave actual del nodo
            print *, "Clave:", nodo%key
            call nodo%matriz%printMatriz()
            ! Imprimir subárbol derecho
            call imprimirRecursivo(nodo%right)
        end if
    end subroutine imprimirRecursivo

    subroutine graficarABB(arbol)
        class(ArbolCapas), intent(in) :: arbol
        character(len=12) :: filename = "arbolABB.dot"
        integer :: fileUnit, iostat

        open(newunit=fileUnit, file=filename, status='replace', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error al abrir el archivo."
            return
        end if

        write(fileUnit, *) "digraph impresiones {"
        if (associated(arbol%raiz)) then
            call escribirNodoRecursivo(arbol%raiz, fileUnit)
        end if
        write(fileunit,*) '}'
        close(fileUnit)
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')   
    end subroutine graficarABB

    recursive subroutine escribirNodoRecursivo(nodo, unitNum)
        type(NodoCapa), pointer, intent(in) :: nodo
        integer, intent(in) :: unitNum

        if (.not. associated(nodo)) return

        ! Escribe el nodo actual
        write(unitNum,*) nodo%key, '[label="Capa ', nodo%key, '"]'
        ! Escribe la arista al hijo izquierdo si existe
        if (associated(nodo%left)) then
            write(unitNum,*) nodo%key, ' -> ', nodo%left%key
            call escribirNodoRecursivo(nodo%left, unitNum)
        end if
        ! Escribe la arista al hijo derecho si existe
        if (associated(nodo%right)) then
            write(unitNum,*) nodo%key, ' -> ', nodo%right%key
            call escribirNodoRecursivo(nodo%right, unitNum)
        end if
    end subroutine escribirNodoRecursivo


end module Arbol_Capas