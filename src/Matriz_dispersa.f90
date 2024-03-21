module matriz_DispersaI
    implicit none

    type :: nodovalor
        private
        logical :: existe = .false.
        character(len = 7) :: valor
    end type nodovalor

    type :: nodo_matriz
        integer :: fila, columna
        character(len=7) :: color
        type(nodo_matriz), pointer :: up => null()
        type(nodo_matriz), pointer :: down => null()
        type(nodo_matriz), pointer :: right => null()
        type(nodo_matriz), pointer :: left => null()
    end type nodo_matriz

    type :: matrizDispersa
        type(nodo_matriz), pointer :: head => null()
        integer :: largo = 0
        integer :: ancho = 0
        contains
            procedure :: searchColumn
            procedure :: searchRow
            procedure :: nodeExits
            procedure :: graficarMatrizDispersa
            procedure :: insertColumnHeader
            procedure :: insertFilaHeader
            procedure :: insertInFila
            procedure :: insertInColumn
            procedure :: mostrarMatriz
            procedure :: printColumnHeaders
            procedure :: agregarMatriz
            procedure :: getValor
            procedure :: vaciarMatriz
            procedure :: generarImagen
    end type matrizDispersa

contains

    subroutine agregarMatriz(self, i, j, color)
        class(matrizDispersa), intent(inout) :: self  
        integer, intent(in) :: i
        integer, intent(in) :: j
        character(len=7), intent(in) :: color

        type(nodo_matriz), pointer :: new
        type(nodo_matriz), pointer :: row
        type(nodo_matriz), pointer :: column

        allocate(new)
        new%fila = i
        new%columna = j
        new%color = color

        if(.not. associated(self%head)) then
            allocate(self%head)
            self%head%fila = -1
            self%head%columna = -1
        end if

        row => self%searchRow(i)
        column => self%searchColumn(j)

        if(j > self%ancho)  self%ancho = j
        if(i > self%largo) self%largo= i

        if(.not. self%nodeExits(new)) then
            if(.not. associated(column)) then
                column => self%insertColumnHeader(j)
            end if

            if(.not. associated(row)) then
                row => self%insertFilaHeader(i)
            end if
            call self%insertInColumn(new, row)
            call self%insertInFila(new, column)
        end if
    end subroutine  agregarMatriz

    function searchColumn(self, j) result(actual)
        class(matrizDispersa), intent(in) :: self
        integer, intent(in) :: j

        type(nodo_matriz), pointer :: actual
        actual => self%head

        do while(associated(actual))
            if(actual%columna == j) return
            actual => actual%right
        end do
    end function searchColumn

    function searchRow(self, i) result(actual)
        class(matrizDispersa), intent(in) :: self
        integer, intent(in) :: i

        type(nodo_matriz), pointer :: actual
        actual => self%head

        do while(associated(actual))
            if(actual%fila == i) return
            actual => actual%down
        end do
    end function searchRow

    function nodeExits(self, new) result(exists)
        class(matrizDispersa), intent(inout) :: self  
        type(nodo_matriz), pointer :: new
        
        logical :: exists
        type(nodo_matriz), pointer :: rowHeader
        type(nodo_matriz), pointer :: column
        rowHeader => self%head
        exists = .false.

        do while(associated(rowHeader))
            if(rowHeader%fila == new%fila) then
                column => rowHeader
                do while(associated(column))
                    if(column%columna == new%columna) then
                        column%color = new%color
                        exists = .true.
                        return
                    end if
                    column => column%right
                end do
                return
            end if
            rowHeader => rowHeader%down
        end do
        return
    end function nodeExits

    function insertFilaHeader(self, i) result(newRowHeader)
        class(matrizDispersa), intent(inout) :: self  
        integer, intent(in) :: i

        type(nodo_matriz), pointer :: newRowHeader
        allocate(newRowHeader)
        newRowHeader%fila = i
        newRowHeader%columna = -1
        call self%insertInFila(newRowHeader, self%head)
    end function insertFilaHeader

    subroutine insertInFila(self, new, rowHeader)
        class(matrizDispersa), intent(inout) :: self
        type(nodo_matriz), pointer :: new
        type(nodo_matriz), pointer :: rowHeader

        type(nodo_matriz), pointer :: actual
        actual => rowHeader

        do while(associated(actual%down))
            if(new%fila < actual%down%fila .and. new%fila > actual%fila) then
                new%down => actual%down
                new%up => actual
                actual%down%up => new
                actual%down => new
                exit
            end if
            actual => actual%down
        end do

        if(.not. associated(actual%down)) then
            actual%down => new
            new%up => actual
        end if
    end subroutine insertInFila

    function insertColumnHeader(self, j) result(newColumnHeader)
        class(matrizDispersa), intent(inout) :: self  
        integer, intent(in) :: j

        type(nodo_matriz), pointer :: newColumnHeader
        allocate(newColumnHeader)
        newColumnHeader%fila = -1
        newColumnHeader%columna = j
        call self%insertInColumn(newColumnHeader, self%head)
    end function insertColumnHeader

    subroutine insertInColumn(self, new, columnHeader)
        class(matrizDispersa), intent(inout) :: self
        type(nodo_matriz), pointer :: new
        type(nodo_matriz), pointer :: columnHeader
        
        type(nodo_matriz), pointer :: actual
        actual => columnHeader
        do while(associated(actual%right))
            if(new%columna < actual%right%columna .and. new%columna > actual%columna) then
                new%right => actual%right
                new%left => actual
                actual%right%left => new
                actual%right => new
                exit
            end if
            actual => actual%right
        end do
        
        if(.not. associated(actual%right)) then
            actual%right => new
            new%left => actual
        end if
    end subroutine insertInColumn

    subroutine mostrarMatriz(self)
        class(matrizDispersa), intent(inout) :: self  
        integer :: i
        integer :: j
        type(nodo_matriz), pointer :: aux
        type(nodovalor) :: val
        aux => self%head%down

        call self%printColumnHeaders()

        do i = 0, self%largo
            print *, ""
            write(*, fmt='(I3)', advance='no') i
            do j = 0, self%ancho
                val = self%getValor(i,j)
                if(.not. val%existe) then
                    write(*, fmt='(I3)', advance='no') 0
                else
                    write(*, fmt='(L3)', advance='no') val%valor
                end if
            end do
        end do
        print *, ""
    end subroutine mostrarMatriz

    subroutine printColumnHeaders(self)
        class(matrizDispersa), intent(in) :: self
        integer :: j

        do j=-1, self%ancho
            write(*, fmt='(I3)', advance='no') j
        end do
    end subroutine printColumnHeaders

    function getValor(self, i, j) result(val)
        class(matrizDispersa), intent(in) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j
        
        type(nodo_matriz), pointer :: rowHeader
        type(nodo_matriz), pointer :: column
        type(nodovalor) :: val
        rowHeader => self%head

        do while(associated(rowHeader))
            if(rowHeader%fila == i) then
                column => rowHeader
                do while(associated(column))
                    if(column%columna == j) then
                        val%valor = column%color
                        val%existe = .true.
                        return
                    end if
                    column => column%right
                end do
                return
            end if
            rowHeader => rowHeader%down
        end do
    end function getValor
    
    subroutine graficarMatrizDispersa(self)
        class(matrizDispersa), intent(inout) :: self  
        character(len=18) :: filename = "capaLogico"
        integer :: i, j, fileUnit, iostat
        type(nodo_matriz), pointer :: aux
        type(nodovalor) :: val
        character(len=256) :: dotPath, pngPath
        aux => self%head%down

        dotPath = 'dot/' // trim(filename) // '.dot'
        pngPath = 'img/' // trim(adjustl(filename))

        print *,"Generando matriz dispersa..."
        open(newunit=fileUnit, file=dotPath, status='replace', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error al abrir el archivo."
            return
        end if

        write(fileUnit, *) "graph matriz {"
        write(fileUnit, *) "    node [shape=box];"

        write(fileUnit,'(A)') '"Origen" [label="-1", group = 1]'

        !Generacion de encabezados de cada columna
        do j=0, self%ancho
            write(fileUnit,'(A,I0,A,I0,A,I0,A)') '"Col', j, '"[label="', j, '", group =', j+2, ']'
            if (.not. j == 0) then
            if (j /= self%ancho) then
                write(fileUnit,'(A,I0,A,I0,A)') '"Col', j, '" --  "Col', j+1, '"'
            end if
            else
                write(fileUnit,'(A,I0,A,I0,A)') '"Origen" --  "Col', j, '"'
                write(fileUnit,'(A,I0,A,I0,A)') '"Col', j, '" --  "Col', j+1, '"'
            end if
        end do

        write(fileUnit, *) ' { rank=same; "Origen";'

        do j=0, self%ancho
            write(fileUnit, '(A,I0,A)', advance='no') ' "Col', j, '";'
        end do

        write(fileUnit, *) " }"

        ! Generacion de encabezados de cada Fila
        do i=0, self%largo
            write(fileUnit,'(A,I0,A,I0,A)') '"Fil', i, '"[label="', i, '", group = 1]'
            if (.not. i == 0) then
            if (i /= self%largo) then
                write(fileUnit,'(A,I0,A,I0,A)') '"Fil', i, '" --  "Fil', i+1, '"'
            end if
            else
                write(fileUnit,'(A,I0,A,I0,A)') '"Origen" --  "Fil', i, '"'
                write(fileUnit,'(A,I0,A,I0,A)') '"Fil', i, '" --  "Fil', i+1, '"'
            end if
        end do

        do i = 0, self%largo
            do j = 0, self%ancho
                val = self%getValor(i,j)
                if(.not. val%existe) then
                    write(fileUnit,'(A,I0,A,I0,A,I0,A)') '"F',i, 'C', j, '" [label=" ", group=',  j+2,']'
                else
                    write(fileUnit, '(A,I0,A,I0,A,A,A)') '"F',i, 'C', j, '" [label=" ", style=filled, fillcolor="', &
                    val%valor, '"]'
                end if
                if (i == 0 .and. j == 0) then
                    write(fileUnit,'(A,I0,A,A,I0,A,I0,A)') '"Fil', i, '" -- ', '"F',i, 'C', j, '"'
                    write(fileUnit,'(A,I0,A,A,I0,A,I0,A)') '"Col', j, '" -- ', '"F',i, 'C', j, '"'
                else if (i == 0) then
                    write(fileUnit,'(A,I0,A,A,I0,A,I0,A)') '"Col', j, '" -- ', '"F',i, 'C', j, '"'
                else if (j == 0) then
                    write(fileUnit,'(A,I0,A,A,I0,A,I0,A)') '"Fil', i, '" -- ', '"F',i, 'C', j, '"'
                end if 
                if (j /= self%ancho) write(fileUnit,'(A,I0,A,I0,A,A,I0,A,I0,A)') '"F',i, 'C', j, '" -- ' , '"F',i, 'C', j+1, '"'
                if (i /= self%largo) write(fileUnit,'(A,I0,A,I0,A,A,I0,A,I0,A)') '"F',i, 'C', j, '" -- ', '"F',i+1, 'C', j, '"'
            end do

            write(fileUnit, '(A,I0,A)') ' { rank=same; "Fil', i, '";'
            do j=0, self%ancho
                write(fileUnit, '(A,I0,A,I0,A)', advance='no') '"F',i, 'C', j, '";'
            end do
            write(fileUnit, *) "}"
        end do

        write(fileUnit, *) "}"
        close(fileUnit)
    
        call system('dot -Tpng ' // trim(dotPath) // ' -o ' // trim(adjustl(pngPath)) // '.png')   
    end subroutine graficarMatrizDispersa

    subroutine vaciarMatriz(self)
        class(matrizDispersa), intent(inout) :: self
        type(nodo_matriz), pointer :: currentRow, nextRow, currentNode, nextNode
        
        if (.not. associated(self%head)) then
            return
        end if
        currentRow => self%head%down
        do while(associated(currentRow))
            ! Guarda el siguiente encabezado de fila antes de liberar el actual.
            nextRow => currentRow%down
            
            ! Recorre y libera todos los nodos en la fila actual.
            currentNode => currentRow%right
            do while(associated(currentNode))
                nextNode => currentNode%right
                nullify(currentNode%up)
                nullify(currentNode%down)
                nullify(currentNode%left)
                deallocate(currentNode)
                currentNode => nextNode
            end do
            
            ! Libera el encabezado de la fila.
            nullify(currentRow%up)
            nullify(currentRow%down)
            deallocate(currentRow)
            currentRow => nextRow
        end do
        
        ! Repite el proceso para los encabezados de columna.
        currentNode => self%head%right
        do while(associated(currentNode))
            nextNode => currentNode%right
            nullify(currentNode%up)
            nullify(currentNode%down)
            nullify(currentNode%left)
            deallocate(currentNode)
            currentNode => nextNode
        end do
    
        ! Restablece las propiedades de la matriz.
        deallocate(self%head)
        self%head => null()
        self%largo = 0
        self%ancho = 0
    end subroutine vaciarMatriz

    subroutine generarImagen(self)
        class(matrizDispersa), intent(inout) :: self  
        character(len=18) :: filename = "imagenGenerada"
        integer :: i, j, fileUnit, iostat
        type(nodo_matriz), pointer :: aux
        type(nodovalor) :: val
        character(len=256) :: dotPath, pngPath
        aux => self%head%down

        dotPath = 'dot/' // trim(filename) // '.dot'
        pngPath = 'img/' // trim(adjustl(filename))

        print *,"Generando imagen..."
        open(newunit=fileUnit, file=dotPath, status='replace', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error al abrir el archivo."
            return
        end if

        write(fileUnit, *) "graph matriz {"
        write(fileUnit, *) "    node [shape=box, style=filled, color=white];"

        write(fileUnit,'(A)') '"Origen" [label="-1", group = 1]'

        !Generacion de encabezados de cada columna
        do j=0, self%ancho
            write(fileUnit,'(A,I0,A,I0,A,I0,A)') '"Col', j, '"[label="', j, '", group =', j+2, ']'
            if (.not. j == 0) then
            if (j /= self%ancho) then
                write(fileUnit,'(A,I0,A,I0,A)') '"Col', j, '" --  "Col', j+1, '" [color=white];'
            end if
            else
                write(fileUnit,'(A,I0,A,I0,A)') '"Origen" --  "Col', j, '" [color=white];'
                write(fileUnit,'(A,I0,A,I0,A)') '"Col', j, '" --  "Col', j+1, '" [color=white];'
            end if
        end do

        write(fileUnit, *) ' { rank=same; "Origen";'

        do j=0, self%ancho
            write(fileUnit, '(A,I0,A)', advance='no') ' "Col', j, '";'
        end do

        write(fileUnit, *) " }"

        ! Generacion de encabezados de cada Fila
        do i=0, self%largo
            write(fileUnit,'(A,I0,A,I0,A)') '"Fil', i, '"[label="', i, '", group = 1]'
            if (.not. i == 0) then
            if (i /= self%largo) then
                write(fileUnit,'(A,I0,A,I0,A)') '"Fil', i, '" --  "Fil', i+1, '" [color=white];'
            end if
            else
                write(fileUnit,'(A,I0,A,I0,A)') '"Origen" --  "Fil', i, '" [color=white];'
                write(fileUnit,'(A,I0,A,I0,A)') '"Fil', i, '" --  "Fil', i+1, '" [color=white];'
            end if
        end do

        do i = 0, self%largo
            do j = 0, self%ancho
                val = self%getValor(i,j)
                if(.not. val%existe) then
                    write(fileUnit,'(A,I0,A,I0,A,I0,A)') '"F',i, 'C', j, '" [label=" ", group=',  j+2,']'
                else
                    write(fileUnit, '(A,I0,A,I0,A,A,A)') '"F',i, 'C', j, '" [label=" ", style=filled, fillcolor="', &
                    val%valor, '"]'
                end if
                if (i == 0 .and. j == 0) then
                    write(fileUnit,'(A,I0,A,A,I0,A,I0,A)') '"Fil', i, '" -- ', '"F',i, 'C', j, '" [color=white];'
                    write(fileUnit,'(A,I0,A,A,I0,A,I0,A)') '"Col', j, '" -- ', '"F',i, 'C', j, '" [color=white];'
                else if (i == 0) then
                    write(fileUnit,'(A,I0,A,A,I0,A,I0,A)') '"Col', j, '" -- ', '"F',i, 'C', j, '" [color=white];'
                else if (j == 0) then
                    write(fileUnit,'(A,I0,A,A,I0,A,I0,A)') '"Fil', i, '" -- ', '"F',i, 'C', j, '" [color=white];'
                end if 
                if (j /= self%ancho) write(fileUnit,'(A,I0,A,I0,A,A,I0,A,I0,A)') '"F',i, 'C', j, '" -- ' , '"F',i, 'C', j+1, &
                '" [color=white];'
                if (i /= self%largo) write(fileUnit,'(A,I0,A,I0,A,A,I0,A,I0,A)') '"F',i, 'C', j, '" -- ', '"F',i+1, 'C', j, &
                '" [color=white];'
            end do

            write(fileUnit, '(A,I0,A)') ' { rank=same; "Fil', i, '";'
            do j=0, self%ancho
                write(fileUnit, '(A,I0,A,I0,A)', advance='no') '"F',i, 'C', j, '";'
            end do
            write(fileUnit, *) "}"
        end do

        write(fileUnit, *) "}"
        close(fileUnit)
    
        call system('dot -Tpng ' // trim(dotPath) // ' -o ' // trim(adjustl(pngPath)) // '.png')   
    end subroutine generarImagen


end module matriz_DispersaI