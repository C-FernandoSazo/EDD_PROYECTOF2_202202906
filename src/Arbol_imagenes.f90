module Arbol_Imagenes
    use Arbol_Capas
    implicit none

    type :: NodoImagen
        integer :: id
        integer :: altura
        type(ArbolCapas) :: arbolCapa
        type(NodoImagen), pointer :: left => null()
        type(NodoImagen), pointer :: right => null()
    end type NodoImagen

    type :: ArbolImagenes
        type(NodoImagen), pointer :: raiz => null()
        contains
            procedure :: insertar
            procedure :: add_recursivo
            procedure :: getAltura
            procedure :: rsi 
            procedure :: rsd
            procedure :: rdi
            procedure :: rdd
            procedure :: getMax
            procedure :: graficarAVL
            procedure :: imprimirArbolImg
            procedure :: ingresarCapas
    end type ArbolImagenes

contains 

    subroutine insertar(this, id)
        class(ArbolImagenes), intent(inout) :: this
        integer, intent(in) :: id
        type(NodoImagen), pointer :: temp

        if(associated(this%raiz)) then
            print *,"ahora vamos a guardar el nodo"
            call this%add_recursivo(id,this%raiz)
        else 
            print *,"RAIZ ES NULL"
            allocate(temp)
            temp%id =  id
            temp%altura = 0
            this%raiz => temp
        end if

    end subroutine insertar

    recursive subroutine add_recursivo(this, id, temp)
        class(ArbolImagenes), intent(inout) :: this
        integer, intent(in) :: id
        type(NodoImagen), pointer, intent(inout) :: temp
        integer :: alturaRight, alturaLeft, m
        print *,"LLAMADA RECURSIVA"
        if (.not. associated(temp)) then
            print *, "Guardando nodo"
            allocate(temp)
            temp%id =  id
            temp%altura = 0
        else if (id < temp%id) then
            print *, "el valor es menor"
            call this%add_recursivo(id,temp%left)
            print *, "Se agrego correctamente"
            if ((this%getAltura(temp%left) - this%getAltura(temp%right)) == 2) then
                if (id < temp%left%id) then
                    temp => this%rsi(temp)
                else 
                    temp => this%rdi(temp)
                end if
            end if
        else 
            print *, "El valor es mayor"
            call this%add_recursivo(id,temp%right)
            print *, "Se agrego correctamente"
            if ((this%getAltura(temp%right) - this%getAltura(temp%left)) == 2) then
                if (id > temp%right%id) then
                    temp => this%rsd(temp)
                else 
                    temp => this%rdd(temp)
                end if
            end if
        end if
        print *,"TERMINO EL CICLO"
        alturaRight = this%getAltura(temp%right)
        alturaLeft = this%getAltura(temp%left)
        m = this%getMax(alturaRight,alturaLeft)
        temp%altura = m + 1
    end subroutine add_recursivo

    integer function getAltura (this, temp)
        class(ArbolImagenes), intent(in) :: this
        type(NodoImagen), intent(in), pointer :: temp
        if (.not. associated(temp)) then
            getAltura = -1
        else
            getAltura = temp%altura
        end if
    end function getAltura

    ! Rotacion simple por la izquierda
    function rsi(this, t1) result(t2)
        class(ArbolImagenes), intent(in) :: this
        type(NodoImagen), intent(in), pointer :: t1
        type(NodoImagen), pointer :: t2 
        t2 => t1%left
        t1%left => t2%right
        t2%right => t1
        t1%altura = this%getMax(this%getAltura(t1%left), this%getAltura(t1%right))+1
        t2%altura = this%getMax(this%getAltura(t2%left), t1%altura)+1
    end function rsi
    
    ! Rotacion simple por la derecha
    function rsd(this, t1) result(t2)
        class(ArbolImagenes), intent(in) :: this
        type(NodoImagen), intent(in), pointer :: t1
        type(NodoImagen), pointer :: t2 
        t2 => t1%right
        t1%right => t2%left
        t2%left => t1
        t1%altura = this%getmax(this%getAltura(t1%left), this%getAltura(t1%right))+1
        t2%altura = this%getmax(this%getAltura(t2%right), t1%altura)+1
    end function rsd

    ! Rotacion doble por la izquierda  
    function rdi(this, tmp) result(res)
        class(ArbolImagenes), intent(in) :: this
        type(NodoImagen), intent(in), pointer :: tmp
        type(NodoImagen), pointer :: res
        tmp%left => this%rsd(tmp%left)
        res => this%rsi(tmp)
    end function rdi

    ! Rotacion doble por la derecha
    function rdd(this, tmp) result(res)
        class(ArbolImagenes), intent(in) :: this
        type(NodoImagen), intent(in), pointer :: tmp
        type(NodoImagen), pointer :: res
        tmp%right => this%rsi(tmp%right)
        res => this%rsd(tmp)
    end function rdd

    integer function getMax(this,val1,val2)
        class(ArbolImagenes), intent(in) :: this
        integer, intent(in) :: val1, val2
        getMax = merge(val1, val2, val1 > val2)
    end function getMax

    subroutine graficarAVL(arbol)
        class(ArbolImagenes), intent(in) :: arbol
        character(len=12) :: filename = "arbolAVL.dot"
        integer :: fileUnit, iostat

        open(newunit=fileUnit, file=filename, status='replace', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error al abrir el archivo."
            return
        end if

        write(fileUnit, *) "digraph impresiones {"
        if (associated(arbol%raiz)) then
            call escribirNodoRecursivos(arbol%raiz, fileUnit)
        end if
        write(fileunit,*) '}'
        close(fileUnit)
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')   
    end subroutine graficarAVL

    recursive subroutine escribirNodoRecursivos(nodo, unitNum)
        type(NodoImagen), pointer, intent(in) :: nodo
        integer, intent(in) :: unitNum

        if (.not. associated(nodo)) return

        ! Escribe el nodo actual
        write(unitNum,'(I0,A,I0,A)') nodo%id, '[label="Imagen ', nodo%id, '"]'
        ! Escribe la arista al hijo izquierdo si existe
        if (associated(nodo%left)) then
            write(unitNum,*) nodo%id, ' -> ', nodo%left%id
            call escribirNodoRecursivos(nodo%left, unitNum)
        end if
        ! Escribe la arista al hijo derecho si existe
        if (associated(nodo%right)) then
            write(unitNum,*) nodo%id, ' -> ', nodo%right%id
            call escribirNodoRecursivos(nodo%right, unitNum)
        end if
    end subroutine escribirNodoRecursivos

    subroutine imprimirArbolImg(arbol)
        class(ArbolImagenes), intent(in) :: arbol
        ! Si el árbol no está vacío, imprimir recursivamente
        if (associated(arbol%raiz)) then
            call imprimirRecursivos(arbol%raiz)
        end if
    end subroutine imprimirArbolImg

    ! Subrutina recursiva para imprimir los nodos en orden
    recursive subroutine imprimirRecursivos(nodo)
        type(NodoImagen), pointer, intent(in) :: nodo
        
        if (associated(nodo)) then
            ! Imprimir subárbol izquierdo
            call imprimirRecursivos(nodo%left)
            ! Imprimir clave actual del nodo
            print *, "Imagen:", nodo%id
            call nodo%arbolCapa%imprimirEnOrden()
            print *, ""
            ! Imprimir subárbol derecho
            call imprimirRecursivos(nodo%right)
        end if
    end subroutine imprimirRecursivos

    subroutine ingresarCapas(arbol, key, keyCapa, matriz) 
        class(ArbolImagenes), intent(in) :: arbol
        integer, intent(in) :: key, keyCapa
        type(matrizDispersa), intent(in) :: matriz

        call capasRecursivo(arbol%raiz, key, keyCapa, matriz)

    end subroutine ingresarCapas

    ! Subrutina recursiva para buscar un nodo
    recursive subroutine capasRecursivo(nodo, key, keyCapa, matriz)
        type(NodoImagen), pointer, intent(in) :: nodo
        integer, intent(in) :: key, keyCapa
        type(matrizDispersa), intent(in) :: matriz

        ! Si la clave buscada es menor que la clave del nodo actual, buscar en el subárbol izquierdo
        if (key < nodo%id) then
            call capasRecursivo(nodo%left, key, keyCapa, matriz)
        ! Si la clave buscada es mayor que la clave del nodo actual, buscar en el subárbol derecho
        else if (key > nodo%id) then
            call capasRecursivo(nodo%right, key, keyCapa, matriz)
        ! Si la clave buscada es igual a la clave del nodo actual, hemos encontrado el nodo
        else
            call nodo%arbolCapa%insertarNodoConMatriz(keyCapa,matriz)
        end if
    end subroutine capasRecursivo
    

end module Arbol_Imagenes