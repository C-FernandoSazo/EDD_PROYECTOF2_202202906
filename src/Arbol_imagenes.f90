module Arbol_ImagenesAVLs
    use Arbol_CapaBB
    implicit none
    ! Arbol AVL

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
            procedure :: buscarImg
            procedure :: graficarAVL_BB
            procedure :: eliminar
            procedure :: eliminarNodo
            procedure :: getMin
            procedure :: contarImagenes
            procedure :: top5Img
            procedure :: agregarArbolCapa
    end type ArbolImagenes

contains 

    subroutine insertar(this, id)
        class(ArbolImagenes), intent(inout) :: this
        integer, intent(in) :: id
        type(NodoImagen), pointer :: temp

        if(associated(this%raiz)) then
            call this%add_recursivo(id,this%raiz)
        else 
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
        if (.not. associated(temp)) then
            allocate(temp)
            temp%id =  id
            temp%altura = 0
        else if (id < temp%id) then
            call this%add_recursivo(id,temp%left)
            if ((this%getAltura(temp%left) - this%getAltura(temp%right)) == 2) then
                if (id < temp%left%id) then
                    temp => this%rsi(temp)
                else 
                    temp => this%rdi(temp)
                end if
            end if
        else 
            call this%add_recursivo(id,temp%right)
            if ((this%getAltura(temp%right) - this%getAltura(temp%left)) == 2) then
                if (id > temp%right%id) then
                    temp => this%rsd(temp)
                else 
                    temp => this%rdd(temp)
                end if
            end if
        end if
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
        character(len=13) :: filename = "arbolImagenes"
        integer :: fileUnit, iostat

        character(len=256) :: dotPath, pngPath

        dotPath = 'dot/' // trim(filename) // '.dot'
        pngPath = 'img/' // trim(adjustl(filename))

        open(newunit=fileUnit, file=dotPath, status='replace', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error al abrir el archivo."
            return
        end if

        write(fileUnit, *) "digraph imagenes {"
        if (associated(arbol%raiz)) then
            call escribirNodoRecursivos(arbol%raiz, fileUnit)
        end if
        write(fileunit,*) '}'
        close(fileUnit)
        call system('dot -Tpng ' // trim(dotPath) // ' -o ' // trim(adjustl(pngPath)) // '.png')    
        call system('start ' // trim(adjustl(pngPath)) // '.png') 
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

    !Codigo para graficar el arbol de imagenes pero con el arbol de capas de un nodo en especifico
    subroutine graficarAVL_BB(arbol,key)
        class(ArbolImagenes), intent(in) :: arbol
        integer, intent(in) :: key
        character(len=14) :: filename = "Imagenes-capa"
        integer :: fileUnit, iostat

        character(len=256) :: dotPath, pngPath

        dotPath = 'dot/' // trim(filename) // '.dot'
        pngPath = 'img/' // trim(adjustl(filename))

        open(newunit=fileUnit, file=dotPath, status='replace', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error al abrir el archivo."
            return
        end if

        write(fileUnit, *) "digraph ALV_BB {"
        if (associated(arbol%raiz)) then
            call escribirRecursivo(arbol%raiz, fileUnit,key)
        end if
        write(fileunit,*) '}'
        close(fileUnit)
        call system('dot -Tpng ' // trim(dotPath) // ' -o ' // trim(adjustl(pngPath)) // '.png')     
        call system('start ' // trim(adjustl(pngPath)) // '.png')
    end subroutine graficarAVL_BB

    recursive subroutine escribirRecursivo(nodo, unitNum, key)
        type(NodoImagen), pointer, intent(in) :: nodo
        integer, intent(in) :: unitNum, key
        integer :: rootCapa

        if (.not. associated(nodo)) return

        ! Escribe el nodo actual
        write(unitNum,'(I0,A,I0,A)') nodo%id, '[label="Imagen ', nodo%id, '"]'
        ! Escribe la arista al hijo izquierdo si existe
        if (nodo%id == key) then
            rootCapa = nodo%arbolCapa%getRaiz()
            call nodo%arbolCapa%escribirABB(unitNum)
            write(unitNum,'(I0,A,I0,A)') nodo%id, ' -> "Capa', rootCapa,'" [color=red];'
        end if
        if (associated(nodo%left)) then
            write(unitNum,*) nodo%id, ' -> ', nodo%left%id
            call escribirRecursivo(nodo%left, unitNum, key)
        end if
        ! Escribe la arista al hijo derecho si existe
        if (associated(nodo%right)) then
            write(unitNum,*) nodo%id, ' -> ', nodo%right%id
            call escribirRecursivo(nodo%right, unitNum, key)
        end if
    end subroutine escribirRecursivo

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
            write(*,'(A,I0)') "Imagen: ", nodo%id
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
    
    function buscarImg(arbol, key) result(imgEncontrada)
        class(ArbolImagenes), intent(in) :: arbol
        integer, intent(in) :: key
        type(NodoImagen) ,pointer :: imgEncontrada

        imgEncontrada => buscarRecursivoImg(arbol%raiz, key)
    end function buscarImg

    ! Subrutina recursiva para buscar un nodo
    recursive function buscarRecursivoImg(nodo, key) result(nodoEncontrado)
        type(NodoImagen), pointer, intent(in) :: nodo
        integer, intent(in) :: key
        type(NodoImagen), pointer :: nodoEncontrado

        ! Si el nodo actual es nulo, no se encontró el nodo buscado
        if (.not. associated(nodo)) then
            nodoEncontrado => null()
            return
        end if

        ! Si la clave buscada es menor que la clave del nodo actual, buscar en el subárbol izquierdo
        if (key < nodo%id) then
            nodoEncontrado => buscarRecursivoImg(nodo%left, key)
        ! Si la clave buscada es mayor que la clave del nodo actual, buscar en el subárbol derecho
        else if (key > nodo%id) then
            nodoEncontrado => buscarRecursivoImg(nodo%right, key)
        ! Si la clave buscada es igual a la clave del nodo actual, hemos encontrado el nodo
        else
            nodoEncontrado => nodo
        end if
    end function buscarRecursivoImg
    
    recursive subroutine eliminar(this, id)
        class(ArbolImagenes), intent(inout) :: this
        integer, intent(in) :: id
        if (.not. associated(this%raiz)) then
            print *, "El árbol está vacío."
        else
            call this%eliminarNodo(this%raiz, id)
            print *,"Imagen eliminada del arbol de imagenes"
        endif
    end subroutine eliminar

    recursive subroutine eliminarNodo(this, temp, id)
        class(ArbolImagenes), intent(inout) :: this
        type(NodoImagen), pointer, intent(inout) :: temp
        type(NodoImagen), pointer :: tempChild, tempMin
        integer, intent(in) :: id
        integer :: alturaRight, alturaLeft, balance
        if (.not. associated(temp)) then
            return
        endif
    
        if (id < temp%id) then
            call this%eliminarNodo(temp%left, id)
        elseif (id > temp%id) then
            call this%eliminarNodo(temp%right, id)
        else
            if (.not. associated(temp%left) .or. .not. associated(temp%right)) then
                if (associated(temp%left)) then
                    tempChild => temp%left
                else
                    tempChild => temp%right
                endif
                if (.not. associated(tempChild)) then
                    tempChild => null()
                    deallocate(temp)
                else
                    temp => tempChild
                endif
            else
                tempMin => this%getMin(temp%right)
                temp%id = tempMin%id
                call this%eliminarNodo(temp%right, tempMin%id)
            endif
        endif
    
        if (.not. associated(temp)) then
            return
        endif
    
        ! Actualizar altura del nodo actual
        temp%altura = 1 + this%getMax(this%getAltura(temp%left), this%getAltura(temp%right))
    
        ! Verificar el balance del nodo actual y realizar las rotaciones necesarias
        balance = this%getAltura(temp%left) - this%getAltura(temp%right)
    
        if (balance > 1) then
            if (this%getAltura(temp%left%left) >= this%getAltura(temp%left%right)) then
                temp => this%rsi(temp)
            else
                temp => this%rdi(temp)
            endif
        elseif (balance < -1) then
            if (this%getAltura(temp%right%right) >= this%getAltura(temp%right%left)) then
                temp => this%rsd(temp)
            else
                temp => this%rdd(temp)
            endif
        endif
    end subroutine eliminarNodo

    function getMin(this,node) result(minValNode)
        class(ArbolImagenes), intent(inout) :: this
        type(NodoImagen), pointer :: node, minValNode
        minValNode => node
        do while(associated(minValNode%left))
            minValNode => minValNode%left
        end do
    end function getMin

    function contarImagenes(this) result(numNodos)
        class(ArbolImagenes), intent(in) :: this
        integer :: numNodos

        numNodos = contarRecursivo(this%raiz)
    end function contarImagenes

    ! Función auxiliar recursiva para contar nodos
    recursive function contarRecursivo(nodo) result(contador)
        type(NodoImagen), pointer, intent(in) :: nodo
        integer :: contador

        if (.not. associated(nodo)) then
            contador = 0
        else
            contador = 1 + contarRecursivo(nodo%left) + contarRecursivo(nodo%right)
        end if
    end function contarRecursivo

    subroutine top5Img(this)
        class(ArbolImagenes), intent(in) :: this
        integer :: top1Id, top2Id, top3Id, top4Id, top5Id
        integer :: top1Capas, top2Capas, top3Capas, top4Capas, top5Capas
    
        top1Id = 0; top2Id = 0; top3Id = 0; top4Id = 0; top5Id = 0
        top1Capas = 0; top2Capas = 0; top3Capas = 0; top4Capas = 0; top5Capas = 0
    
        call topRecursivo(this%raiz, top1Id, top2Id, top3Id, top4Id, top5Id, &
                        top1Capas, top2Capas, top3Capas, top4Capas, top5Capas)
    
        print *, "Top 5 imagenes con mas numero de capas:"
        if (top1Capas > 0) print *, "1. Imagen ID:", top1Id, "Capas:", top1Capas
        if (top2Capas > 0) print *, "2. Imagen ID:", top2Id, "Capas:", top2Capas
        if (top3Capas > 0) print *, "3. Imagen ID:", top3Id, "Capas:", top3Capas
        if (top4Capas > 0) print *, "4. Imagen ID:", top4Id, "Capas:", top4Capas
        if (top5Capas > 0) print *, "5. Imagen ID:", top5Id, "Capas:", top5Capas
    end subroutine top5Img
    
    recursive subroutine topRecursivo(nodo, top1Id, top2Id, top3Id, top4Id, top5Id, &
        top1Capas, top2Capas, top3Capas, top4Capas, top5Capas)
        type(NodoImagen), pointer, intent(in) :: nodo
        integer, intent(inout) :: top1Id, top2Id, top3Id, top4Id, top5Id
        integer, intent(inout) :: top1Capas, top2Capas, top3Capas, top4Capas, top5Capas
        integer :: currentCapas

        if (.not. associated(nodo)) return

        currentCapas = nodo%arbolCapa%contarCapas() 

        if (currentCapas > top1Capas) then
        top5Id = top4Id; top5Capas = top4Capas
        top4Id = top3Id; top4Capas = top3Capas
        top3Id = top2Id; top3Capas = top2Capas
        top2Id = top1Id; top2Capas = top1Capas
        top1Id = nodo%id; top1Capas = currentCapas
        else if (currentCapas > top2Capas) then
        top5Id = top4Id; top5Capas = top4Capas
        top4Id = top3Id; top4Capas = top3Capas
        top3Id = top2Id; top3Capas = top2Capas
        top2Id = nodo%id; top2Capas = currentCapas
        else if (currentCapas > top3Capas) then
        top5Id = top4Id; top5Capas = top4Capas
        top4Id = top3Id; top4Capas = top3Capas
        top3Id = nodo%id; top3Capas = currentCapas
        else if (currentCapas > top4Capas) then
        top5Id = top4Id; top5Capas = top4Capas
        top4Id = nodo%id; top4Capas = currentCapas
        else if (currentCapas > top5Capas) then
        top5Id = nodo%id; top5Capas = currentCapas
        end if

        call topRecursivo(nodo%left, top1Id, top2Id, top3Id, top4Id, top5Id, &
        top1Capas, top2Capas, top3Capas, top4Capas, top5Capas)
        call topRecursivo(nodo%right, top1Id, top2Id, top3Id, top4Id, top5Id, &
        top1Capas, top2Capas, top3Capas, top4Capas, top5Capas)
        end subroutine topRecursivo

        subroutine agregarArbolCapa(arbol, key, capaTemp) 
            class(ArbolImagenes), intent(in) :: arbol
            integer, intent(in) :: key
            type(ArbolCapas), intent(in) :: capaTemp
    
            call agregarArbolCapaRecursivo(arbol%raiz,key,capaTemp)
        end subroutine agregarArbolCapa
    
        ! Subrutina recursiva para buscar un nodo
        recursive subroutine agregarArbolCapaRecursivo(nodo, key, capaTemp) 
            type(NodoImagen), pointer, intent(in) :: nodo
            integer, intent(in) :: key
            type(ArbolCapas), intent(in) :: capaTemp
    
            ! Si el nodo actual es nulo, no se encontró el nodo buscado
            if (.not. associated(nodo)) then
                return
            end if
    
            ! Si la clave buscada es menor que la clave del nodo actual, buscar en el subárbol izquierdo
            if (key < nodo%id) then
                call agregarArbolCapaRecursivo(nodo%left,key,capaTemp)
            ! Si la clave buscada es mayor que la clave del nodo actual, buscar en el subárbol derecho
            else if (key > nodo%id) then
                call agregarArbolCapaRecursivo(nodo%right,key,capaTemp)
            ! Si la clave buscada es igual a la clave del nodo actual, hemos encontrado el nodo
            else
                nodo%arbolCapa = capaTemp
            end if
        end subroutine agregarArbolCapaRecursivo

end module Arbol_ImagenesAVLs