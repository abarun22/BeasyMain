module BoundarySolutionCalculator_Module
    use, intrinsic :: iso_fortran_env;
    use DataStructure_Module;
    use Utility_Module;                                        !==> TO DELETE?
    implicit none;
    private;
    public :: DDDMS_BoundarySolutionCalculator;
    public :: DDDMS_SMBoundarySolutionCalculator; 
    
    !╔═══════════════════════════════════════════╗
    !║DDDMS_BOUNDARYSOLUTIONCALCULATOR DEFINITION║
    !╚═══════════════════════════════════════════╝
    !┌───────────────────────────────────────────────────────────────────────────┐
    !│ABSTRACT DDDMS_BOUNDARYSOLUTIONCALCULATOR USER-DEFINED DATA TYPE DEFINITION│
    !└───────────────────────────────────────────────────────────────────────────┘
    type, abstract :: DDDMS_BoundarySolutionCalculator
        
    contains 
        procedure(CalculateABTRACT), deferred :: calculate;
        
    end type DDDMS_BoundarySolutionCalculator

    !┌─────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_BOUNDARYSOLUTIONCALCULATOR ABSTRACT USER-DEFINED DATA TYPE PROCEDURE DEFINITION│
    !└─────────────────────────────────────────────────────────────────────────────────────┘
    abstract interface
        subroutine CalculateABTRACT(self, inputData, zonesBounSol, inputParamsData)
            import;
            class(DDDMS_BoundarySolutionCalculator) :: self;
            class(DDDMS_DataContainer)              :: inputData;
            integer(INT32), allocatable             :: zonesBounSol(:);
            class(DDDMS_InputParams)                :: inputParamsData;
        end subroutine CalculateABTRACT
    end interface

    !┌────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    !│CONCRETE DDDMS_SMBOUNDARYSOLUTIONCALCULATOR USER-DEFINED DATA TYPE DEFINITION FOR SHARED MEMORY ARCHITECTURE│
    !└────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    type, extends(DDDMS_BoundarySolutionCalculator) :: DDDMS_SMBoundarySolutionCalculator
        
    contains 
        procedure :: calculate => calculateOnSM;
        
    end type DDDMS_SMBoundarySolutionCalculator

    !┌────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_SMLOCALSCHURCOMPLEMENTCALCULATOR USER-DEFINED DATA TYPE CONSTRUCTOR DEFINITION│
    !└────────────────────────────────────────────────────────────────────────────────────┘
    interface DDDMS_SMBoundarySolutionCalculator
        module procedure NewDDDMS_SMBoundarySolutionCalculator; ! ADD CONSTRUCTOR TO DDDMS_SMBoundarySolutionCalculator GENERIC INTERFACE
    end interface DDDMS_SMBoundarySolutionCalculator

contains

    !┌────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_SMBoundarySolutionCalculator USER-DEFINED DATA TYPE CONSTRUCTOR IMPLEMENTATION│
    !└────────────────────────────────────────────────────────────────────────────────────┘
    type(DDDMS_SMBoundarySolutionCalculator) function NewDDDMS_SMBoundarySolutionCalculator(self)
    
        implicit none;
        
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        class(DDDMS_SMBoundarySolutionCalculator) :: self;
        
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘
        
    end function NewDDDMS_SMBoundarySolutionCalculator

    !┌──────────────────────────────────┐
    !│CALCULATE PROCEDURE IMPLEMENTATION│
    !└──────────────────────────────────┘
    subroutine calculateOnSM(self, inputData, zonesBounSol, inputParamsData)
    
        implicit none;
    
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        class(DDDMS_SMBoundarySolutionCalculator) :: self;
        class(DDDMS_DataContainer)                :: inputData;
        class(DDDMS_InputParams)                  :: inputParamsData;
        integer(INT32), allocatable               :: zonesBounSol(:);
        integer(INT64), allocatable               :: pivot(:);
        integer(INT64), allocatable               :: indices(:);
        integer(INT64), allocatable               :: indices2(:);
        integer(INT32)                            :: info;
        integer(INT32)                            :: status;
        integer(INT32)                            :: i;
        integer(INT64)                            :: j;
        integer(INT32)                            :: k;
        integer(INT32)                            :: currZone;
        integer(INT64)                            :: IndepInterfNodesCount;
        integer(INT64)                            :: countStep;
        integer(INT64)                            :: scaleIndexVal;
        real(REAL64), allocatable                 :: uL(:);
        real(REAL64), allocatable                 :: DispSol(:, :);
        real(REAL64), allocatable                 :: TracSol(:, :);
        real(REAL64), allocatable                 :: TempMAT_(:, :);          !==>VARIABLE TO DELETE?
        real(REAL64), allocatable                 :: TempVEC_(:);             !==>VARIABLE TO DELETE?
        real(REAL64)                              :: ALPHA;
        real(REAL64)                              :: BETA;
        real(REAL64)                              :: deforScaleFact = 1000.0;        
        character(len = 4)                        :: seqstring;               !==> VARIABLE TO DELETE?
! APB        
        integer(INT64)                            :: icountD,icountT,icount;
! APB        
         
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘

        !┌───────────────────────────────────────────────────────────────────────┐
        !│LOOP OVER ZONES IN THE MODEL FOR WHICH A BOUNDARY SOLUTION WAS REQUIRED│
        !└───────────────────────────────────────────────────────────────────────┘        
        do i = 1, size( zonesBounSol )
  
            currZone              = zonesBounSol(i);
            scaleIndexVal         = minval( inputData%ZonesData( currZone )%CoordsNodesInZone(:, 1) );
            IndepInterfNodesCount = count( inputData%IndependenInterfaceNodesArray(:, currZone) /= 0 ); !NUMBER OF INDEPENDENT NODES LYING ON THE INTERFACE FOR THE CURRENT ZONE
            
            allocate(indices, SOURCE = pack( inputData%IndependenInterfaceNodesArray(:, currZone),  &
                              MASK   = inputData%IndependenInterfaceNodesArray(:, currZone) /= 0 ), & 
                              STAT   = status);
            
            if(status /= 0) then
                print*, "Failed allocation of indices array!";
                print*, "Errore code: ", status;
                pause;
                stop;
            end if  
           
            indices = (indices - scaleIndexVal + 1 );

            allocate(indices2, SOURCE = pack( [ (j, j = 1,size( inputData%IndependenInterfaceNodesArray(:, currZone) ) ) ], &
                               MASK   = inputData%IndependenInterfaceNodesArray(:, currZone) /= 0 ),                        &
                               STAT   = status);
            
             if(status /= 0) then
                print*, "Failed allocation of indices2 array!";
                print*, "Errore code: ", status;
                pause;
                stop;
            end if  
           
            !┌───────────────────────────────────────────────────────────────────────────────────────┐
            !│ALLOCATE TEMPORARY ARRAY "UL" TO COLLECT INTERFACE SOLUTION CONCERNING THE CURRENT ZONE│
            !└───────────────────────────────────────────────────────────────────────────────────────┘
            allocate( UL( 3*IndepInterfNodesCount ), STAT = status );
            if(status /= 0) then
                print*, "Failed allocation of UL array!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                uL(:) = 0.0;
            end if  

            !┌───────────────────────────────────────────────────────────────────────────────┐
            !│COLLECT INTERFACE SOLUTION FOR THE CURRENT ZONE FROM VECTOR U_INTERFACESOLUTION│
            !└───────────────────────────────────────────────────────────────────────────────┘
            write(10,*)'Zonal interface index'
            countStep = 1;
            do j = 1, size( inputData%IndependenInterfaceNodesArray(:, currZone), 1) 
                !write(10,*)'Size of IndependenInterfaceNodesArray:','currZone',currZone
                !write(10,*)3*size( inputData%IndependenInterfaceNodesArray(:,currZone),1)
                if( inputData%IndependenInterfaceNodesArray(j, currZone) /= 0 ) then                    
                    UL((3*countStep - 2):(3*countStep)) = inputData%U_InterfaceSolution( (3*j - 2):(3*j) );                    
                    write(10,*)(3*j - 2),(3*j)
                    countStep = countStep + 1;                    
                end if                
            end do            
        
            !┌──────────────────────────────────────────────────────────────┐
            !│(1)--DGEMV: COMPUTES MATRIX-VECTOR PRODUCT: CB <= (CB - DL*UL)│
            !└──────────────────────────────────────────────────────────────┘
            ALPHA = -1.0; 
            BETA  = +1.0;

            call DGEMV('N',                                            & !MATRICES A NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                       size( inputData%ZonesData( currZone )%DL, 1 ),  & !SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
                       size( inputData%ZonesData( currZone )%DL, 2 ),  & !SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
                       ALPHA,                                          & !SPECIFIES THE SCALAR ALPHA.
                       inputData%ZonesData( currZone )%DL,             & !MATRIX A, SIZE (LDA, N).
                       size( inputData%ZonesData( currZone )%DL, 1 ),  & !LEADING DIMENSION OF ARRAY A.
                       UL,                                             & !VECTOR X.
                       1,                                              & !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF X.
                       BETA,                                           & !SPECIFIES THE SCALAR BETA. 
                       inputData%ZonesData( currZone )%CB,             & !VECTOR Y.
                       1 );                                              !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF Y.
            
            !─────────────────────────────────────────────────────           
            !====================TO DELETE========================
            !write (seqstring,'(I0)') currZone;
            !call Utility_OutputDataVector( "Output_FORTRAN_(CB_DL_UL)_Zone_"//trim( seqstring )//".out", &
            !                               inputData%ZonesData( currZone )%CB );

            !====================TO DELETE========================
            !─────────────────────────────────────────────────────

            !┌──────────────────────────────────────────────────────────────────┐
            !│(2)--DGEMV: COMPUTES MATRIX-VECTOR PRODUCT: XB <= B0L*(CB - DL*UL)│
            !└──────────────────────────────────────────────────────────────────┘
            ALPHA = +1.0; 
            BETA  = +0.0;

            call DGEMV('N',                                            & !MATRICES A NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                       size( inputData%ZonesData( currZone )%B0L, 1 ), & !SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
                       size( inputData%ZonesData( currZone )%B0L, 2 ), & !SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
                       ALPHA,                                          & !SPECIFIES THE SCALAR ALPHA.
                       inputData%ZonesData( currZone )%B0L,            & !MATRIX A, SIZE (LDA, N).
                       size( inputData%ZonesData( currZone )%B0L, 1 ), & !LEADING DIMENSION OF ARRAY A.
                       inputData%ZonesData( currZone )%CB,             & !VECTOR X.
                       1,                                              & !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF X.
                       BETA,                                           & !SPECIFIES THE SCALAR BETA. 
                       inputData%ZonesData( currZone )%XB,             & !VECTOR Y.
                       1 );                                              !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF Y.

            !─────────────────────────────────────────────────────           
            !====================TO DELETE========================
            !write (seqstring,'(I0)') currZone;
            !call Utility_OutputDataVector( "Output_FORTRAN_(B0L_(CB_DL_UL)_)_Zone_"//trim( seqstring )//".out", &
            !                               inputData%ZonesData( currZone )%XB );

            !====================TO DELETE========================
            !─────────────────────────────────────────────────────
            
            !┌─────────────────────────────────────────────────────────────┐
            !│(3)--DGEMV: COMPUTES MATRIX-VECTOR PRODUCT: XB <= XB - A0L*UL│
            !└─────────────────────────────────────────────────────────────┘
            ALPHA = -1.0; 
            BETA  = +1.0;

            call DGEMV('N',                                            & !MATRICES A NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                       size( inputData%ZonesData( currZone )%A0L, 1 ), & !SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
                       size( inputData%ZonesData( currZone )%A0L, 2 ), & !SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
                       ALPHA,                                          & !SPECIFIES THE SCALAR ALPHA.
                       inputData%ZonesData( currZone )%A0L,            & !MATRIX A, SIZE (LDA, N).
                       size( inputData%ZonesData( currZone )%A0L, 1 ), & !LEADING DIMENSION OF ARRAY A.
                       UL,                                             & !VECTOR X.
                       1,                                              & !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF X.
                       BETA,                                           & !SPECIFIES THE SCALAR BETA. 
                       inputData%ZonesData( currZone )%XB,             & !VECTOR Y.
                       1 );                                              !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF Y.

            !─────────────────────────────────────────────────────           
            !====================TO DELETE========================
            !write (seqstring,'(I0)') currZone;
            !call Utility_OutputDataVector( "Output_FORTRAN_(B0L_(CB_DL_UL)_(A0L_UL_)_)_Zone_"//trim( seqstring )//".out", &
            !                               inputData%ZonesData( currZone )%XB );

            !====================TO DELETE========================
            !─────────────────────────────────────────────────────
            
            !┌─────────────────────────────────────────────────────────────┐
            !│(4)--DGEMV: COMPUTES MATRIX-VECTOR PRODUCT: XB <= XB + B00*YB│
            !└─────────────────────────────────────────────────────────────┘
            ALPHA = +1.0; 
            BETA  = +1.0;

            call DGEMV('N',                                            & !MATRICES A NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                       size( inputData%ZonesData( currZone )%B00, 1 ), & !SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
                       size( inputData%ZonesData( currZone )%B00, 2 ), & !SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
                       ALPHA,                                          & !SPECIFIES THE SCALAR ALPHA.
                       inputData%ZonesData( currZone )%B00,            & !MATRIX A, SIZE (LDA, N).
                       size( inputData%ZonesData( currZone )%B00, 1 ), & !LEADING DIMENSION OF ARRAY A.
                       inputData%ZonesData( currZone )%Y,              & !VECTOR X.
                       1,                                              & !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF X.
                       BETA,                                           & !SPECIFIES THE SCALAR BETA. 
                       inputData%ZonesData( currZone )%XB,             & !VECTOR Y.
                       1 );                                              !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF Y.

            !─────────────────────────────────────────────────────           
            !====================TO DELETE========================
            !write (seqstring,'(I0)') currZone;
            !call Utility_OutputDataVector( "Output_FORTRAN_(B00_YB_(B0L_(CB_DL_UL)_(A0L_UL_)_)_)_Zone_"//trim( seqstring )//".out", &
            !                               inputData%ZonesData( currZone )%XB );

            !====================TO DELETE========================
            !─────────────────────────────────────────────────────
            
            !┌───────────────────────────────────────────────────────────────┐
            !│(5)--DGEMV: COMPUTES MATRIX-VECTOR PRODUCT: XB <= (A00)^(-1)*XB│
            !└───────────────────────────────────────────────────────────────┘
            !┌─────────────────────────────────────────┐
            !│ALLOCATE TEMPORARY SUPPORT ARRAY TEMPVEC_│
            !└─────────────────────────────────────────┘
            allocate( TempVEC_( size( inputData%ZonesData( currZone )%XB, 1 ) ), STAT = status );
            if(status /= 0) then
                print*, "Allocation of array TempVEC_ failed!.";
                print*, "Error code: ", status;
                pause;
                stop;
            else
                TempVEC_(:) = 0.0;
            end if

            ALPHA = +1.0; 
            BETA  = +0.0;

            call DGEMV('N',                                            & !MATRICES A NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                       size( inputData%ZonesData( currZone )%A00, 1 ), & !SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
                       size( inputData%ZonesData( currZone )%A00, 2 ), & !SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
                       ALPHA,                                          & !SPECIFIES THE SCALAR ALPHA.
                       inputData%ZonesData( currZone )%A00,            & !MATRIX A, SIZE (LDA, N).
                       size( inputData%ZonesData( currZone )%A00, 1 ), & !LEADING DIMENSION OF ARRAY A.
                       inputData%ZonesData( currZone )%XB,             & !VECTOR X.
                       1,                                              & !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF X.
                       BETA,                                           & !SPECIFIES THE SCALAR BETA. 
                       TempVEC_,                                       & !VECTOR Y.
                       1 );                                              !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF Y.

            !┌─────────────────────────────────────────────────────┐
            !│FILL THE SUB-MATRIX XB USING NEW VALUES (A00)^(-1)*XB│
            !└─────────────────────────────────────────────────────┘
            inputData%ZonesData( currZone )%XB = TempVEC_;
            
            write(10,*)'Size of boundary solution array:'
            write(10,*)'currZone, size(XB)', currZone, size(TempVEC_)
            
            !┌───────────────────────────────────────────┐
            !│DEALLOCATE TEMPORARY SUPPORT ARRAY TEMPMAT_│
            !└───────────────────────────────────────────┘
            if( allocated( TempVEC_ ) ) then
                deallocate( TempVEC_ );
                if(status /= 0) then
                    print*, "Deallocation of array TempVEC_ failed!.";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if
            
!            write(10,*)inputData%ZonesData(currZone)%XB
!           inputData%
            
            !┌─────────────────────────────────────────────────────┐
            !│FILL THE MATRIX CONTAINING FINAL DEFORMED NODE VALUES│
            !└─────────────────────────────────────────────────────┘ 
            !┌────────────────────────────────────────┐
            !│ALLOCATE TEMPORARY SUPPORT ARRAY DISPSOL│
            !└────────────────────────────────────────┘
            allocate( DispSol( size( inputData%ZonesData( currZone )%CoordsNodesInZone, 1 ), 4 ), STAT = status );
            if(status /= 0) then
                print*, "Allocation of array DispSol failed!.";
                print*, "Error code: ", status;
                pause;
                stop;
            else
                DispSol(:, :) = inputData%ZonesData( currZone )%CoordsNodesInZone(:, :);
            end if
            
            write(10,*)'Size of allocated displacement array'
            write(10,*)size( inputData%ZonesData( currZone )%CoordsNodesInZone, 1 )
            
            !┌────────────────────────────────────────┐
            !│ALLOCATE TEMPORARY SUPPORT ARRAY TRACSOL│
            !└────────────────────────────────────────┘
            allocate( TracSol( size( inputData%ZonesData( currZone )%CoordsNodesInZone, 1 ), 4 ), STAT = status );
            if(status /= 0) then
                print*, "Allocation of array TracSol failed!.";
                print*, "Error code: ", status;
                pause;
                stop;
            else
                TracSol(:, :) = 0.0;
            end if

            !┌───────────────────────────────────────────────────────────────┐
            !│UPDATE NODES DISPLACEMENT VALUES USING BOUNDARY SOLUTION VALUES│
            !└───────────────────────────────────────────────────────────────┘
            !
            icount=0;
            do j = 1,size( inputData%ZonesData( currZone )%BCsType, 1 ) 
                
                if( inputData%ZonesData( currZone )%BCsType(j, 1) == 5 ) then
!                    TracSol(j, 2) = ( inputParamsData%gMatScaleFact*inputData%ZonesData( currZone )%XB( 3*j - 2) );    
!                    DispSol(j, 2)=0;
                    icount=icount+1
                    
                elseif( inputData%ZonesData( currZone )%BCsType(j, 1) == 8 ) then
                    DispSol(j, 2) = DispSol(j, 2) + ( deforScaleFact*inputData%ZonesData( currZone )%XB( 3*j - 2) );
                    icount=icount+1
                end if
                
                if( inputData%ZonesData( currZone )%BCsType(j, 2) == 6 ) then
                    !TracSol(j, 3) = ( inputParamsData%gMatScaleFact*inputData%ZonesData( currZone )%XB( 3*j - 1) );    
!                    DispSol(j, 3)=0;
                    icount=icount+1                    
                elseif( inputData%ZonesData( currZone )%BCsType(j, 2) == 9 ) then
                    DispSol(j, 3) = DispSol(j, 3) + ( deforScaleFact*inputData%ZonesData( currZone )%XB( 3*j - 1) );    
                    icount=icount+1
                end if
                
                if( inputData%ZonesData( currZone )%BCsType(j, 3) == 7 ) then
                    !TracSol(j, 4) = ( inputParamsData%gMatScaleFact*inputData%ZonesData( currZone )%XB( 3*j ) );    
!                    DispSol(j, 4)=0;
                    icount=icount+1
                    
                elseif( inputData%ZonesData( currZone )%BCsType(j, 3) == 10 ) then
                    DispSol(j, 4) = DispSol(j, 4) + ( deforScaleFact*inputData%ZonesData( currZone )%XB( 3*j ) );                
                    icount=icount+1
                end if
            
            end do

            write(10,*)'icount',icount
            write(10,*)'scaleIndexVal',scaleIndexVal
            write(10,*)'Size of %BCsType array:'
            write(10,*)'currZone, size(BCsType)', currZone, size(inputData%ZonesData( currZone )%BCsType)
            
            !write(10,*)'IndependenInterfaceNodesArray array:'
            !write(10,*)inputData%IndependenInterfaceNodesArray(:, currZone)
            
            write(10,*)'Indices: Interface nodes'
            write(10,*) indices
            
            write(10,*)'Indices2: Interface nodes'
            write(10,*) indices2
            
            !┌────────────────────────────────────────────────────────────────┐
            !│UPDATE NODES DISPLACEMENT VALUES USING INTERFACE SOLUTION VALUES│
            !└────────────────────────────────────────────────────────────────┘
            !X-COMPONENT
            DispSol(indices, 2) = DispSol(indices, 2) + ( deforScaleFact*inputData%U_InterfaceSolution( 3*indices2 - 2 ) );
            
            !Y-COMPONENT
            DispSol(indices, 3) = DispSol(indices, 3) + ( deforScaleFact*inputData%U_InterfaceSolution( 3*indices2 - 1 ) );
            
            !Z-COMPONENT
            DispSol(indices, 4) = DispSol(indices, 4) + ( deforScaleFact*inputData%U_InterfaceSolution( 3*indices2 - 0 ) );

            write(10,*)'Size of indices2 array'
            write(10,*)size(indices2)
            
            !────────────────────────────────────────────────────────           
            !====================TO print out========================
            write (seqstring,'(I0)')currZone;
            call Fortran2VTK( "Output_FORTRAN2VTK_Boundary_Solution_Zone_"//trim( seqstring )//".vtk", DispSol );
            
            write(10,*)'Size of VTK array'
            write(10,*)size(DispSol(:,2))
            !====================TO print out========================
            !────────────────────────────────────────────────────────    
            
            !┌──────────────────────────────────────────┐
            !│DEALLOCATE TEMPORARY SUPPORT ARRAY DISPSOL│
            !└──────────────────────────────────────────┘
            if( allocated( DispSol ) ) then
                deallocate( DispSol );
                if(status /= 0) then
                    print*, "Deallocation of array DispSol failed!.";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if

            !┌──────────────────────────────────────────┐
            !│DEALLOCATE TEMPORARY SUPPORT ARRAY TRACSOL│
            !└──────────────────────────────────────────┘
            if( allocated( TracSol ) ) then
                deallocate( TracSol );
                if(status /= 0) then
                    print*, "Deallocation of array TracSol failed!.";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if

            !┌─────────────────────────────────────────────────────────────────────────────────────────┐
            !│DEALLOCATE TEMPORARY ARRAY "UL" TO COLLECT INTERFACE SOLUTION CONCERNING THE CURRENT ZONE│
            !└─────────────────────────────────────────────────────────────────────────────────────────┘
            if( allocated( uL ) ) then
                deallocate( uL );
                if(status /= 0) then
                    print*, "Deallocation of array uL failed!.";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if
            
            !┌──────────────────────────────────────────┐
            !│DEALLOCATE TEMPORARY SUPPORT ARRAY INDICES│
            !└──────────────────────────────────────────┘
            if( allocated( Indices ) ) then
                deallocate( Indices );
                if(status /= 0) then
                    print*, "Deallocation of array Indices failed!.";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if
            
            !┌───────────────────────────────────────────┐
            !│DEALLOCATE TEMPORARY SUPPORT ARRAY INDICES2│
            !└───────────────────────────────────────────┘
            if( allocated( Indices2 ) ) then
                deallocate( Indices2 );
                if(status /= 0) then
                    print*, "Deallocation of array Indices2 failed!.";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if

        end do  !END OF LOOP STATEMENT OVER ZONES IN THE MODEL FOR WHICH A BOUNDARY SOLUTION WAS REQUIRED  

    end subroutine calculateOnSM    
    
end module BoundarySolutionCalculator_Module
    