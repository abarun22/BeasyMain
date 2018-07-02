!  BEASYMain.f90 
!
!  FUNCTIONS:
!  BEASYMain - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: BEASYMain
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program DirectDDMMain
        use iso_fortran_env;
        use Utility_Module;     
        use DataStructure_Module;
        use SolverController_Module;
        use DataLoader_Module;
        use LocalSchurComplementCalculator_Module;
        use GlobalSchurComplementAssembler_Module;
        use InterfaceSolutionCalculator_Module;
        use BoundarySolutionCalculator_Module;
        use getResultsVector_Module;
        implicit none;
        
        !╔════════════════╗
        !║DECLARATIVE ZONE║
        !╚════════════════╝
        character(len = 200)                                     :: inputSolverPathName;      !==>VARIABLE TO DELETE?
        character(len = 200)                                     :: inputModelFileName;       !==>VARIABLE TO DELETE?
        character(len = 200)                                     :: inputPathFileName;        !==>VARIABLE TO DELETE?
        character(len = 200)                                     :: inputBlockFileName;       !==>VARIABLE TO DELETE?
        character(len = 200)                                     :: currentLine;              !==>VARIABLE TO DELETE?
        character(len = 100)                                     :: currCommandArg;           !==>VARIABLE TO DELETE?
        character(len = 200)                                     :: commandLine;              !==>VARIABLE TO DELETE?
        character(len = 100),                        allocatable :: batchFileCommands(:);
        integer(INT32)                                           :: HPC_Arch;
        integer(INT64)                                           :: i;
        integer(INT64)                                           :: j;
        integer(INT64)                                           :: k;
        integer(INT32)                                           :: status;
        integer(INT64)                                           :: count;
        integer(INT32)                                           :: unit
        integer(INT64)                                           :: file_size;
        integer(INT64)                                           :: file_sizeMod;
        integer(INT64)                                           :: file_sizePath;
        integer(INT64)                                           :: file_sizeBlock;
        integer(INT32)                                           :: file_sizeSolver;
        integer(INT32)                                           :: indexPath;
        integer(INT32),                              allocatable :: zonesBounSol(:);
        integer(INT32)                                           :: currBatchCommand;
        logical                                                  :: file_exists;
        logical                                                  :: file_existsMod;
        logical                                                  :: file_existsPath;
        logical                                                  :: file_existsBlock;
        logical                                                  :: file_existsSolver;
        logical,                                     allocatable :: chechBatchCommandsSatus(:);
        class(DDDMS_SolverController),               allocatable :: controller;
        class(DDDMS_DataLoaderFromFile),             allocatable :: loader;
        type(DDDMS_DataContainer),                   allocatable :: dataContainer;
        class(DDDMS_InputParams),                    allocatable :: inputParamsData;    
        class(DDDMS_LocalSchurComplementCalculator), allocatable :: LocSchurComp_Calculator;
        class(DDDMS_GlobalSchurComplementAssembler), allocatable :: GloSchurComp_Assembler;
        class(DDDMS_InterfaceSolutionCalculator),    allocatable :: InterSol_Calculator;
        class(DDDMS_BoundarySolutionCalculator),     allocatable :: BoundSol_Calculator;
        class(DDDMS_getResultsVector),               allocatable :: ResVec;
        real(REAL64)                                             :: start_time;
        real(REAL64)                                             :: stop_time;
        integer(INT64)                                           :: nzones;
        
        !╔══════════════════════════════════════════════════════════════════╗
        !║CALL UTILITY PROCEDURE TO COLLECT DATA FROM COMMAND LINE ARGUMENTS║
        !╚══════════════════════════════════════════════════════════════════╝
        call Utility_CommandParameters(inputSolverPathName, &
                                       inputModelFileName,  &
                                       inputPathFileName,   &
                                       inputBlockFileName,  &
                                       batchFileCommands,   &
                                       HPC_Arch);     

 
        inquire( FILE = inputModelFileName , EXIST = file_existsMod );     !CHECK IF THE INPUT MODEL FILE INPUTMODELFILENAME EXISTS.
        inquire( FILE = inputModelFileName , SIZE = file_sizeMod );        !CHECK IF THE INPUT MODEL FILE INPUTMODELFILENAME IS EMPTY.

        inquire( FILE = inputPathFileName , EXIST = file_existsPath );     !CHECK IF THE INPUT FILE INPUTPATHFILENAME EXISTS.
        inquire( FILE = inputPathFileName , SIZE = file_sizePath );        !CHECK IF THE INPUT FILE INPUTPATHFILENAME IS EMPTY.

        inquire( FILE = inputBlockFileName , EXIST = file_existsBlock );   !CHECK IF THE DUMP SIZE INPUT FILE INPUTBLOCKFILENAME EXISTS.
        inquire( FILE = inputBlockFileName , SIZE = file_sizeBlock );      !CHECK IF THE DUMP SIZE INPUT FILE INPUTBLOCKFILENAME IS EMPTY.

        inquire( FILE = inputSolverPathName , EXIST = file_existsSolver ); !CHECK IF THE DUMP SIZE INPUT FILE INPUTBLOCKFILENAME EXISTS.
        inquire( FILE = inputSolverPathName , SIZE = file_sizeSolver );    !CHECK IF THE DUMP SIZE INPUT FILE INPUTBLOCKFILENAME IS EMPTY.

        if( ( HPC_Arch == 1 ) .or. ( HPC_Arch == 2 )         .AND. &   !HPC ARCHITECTURE CAN BE SHARED MEMORY OR DISTRIBUTED MEMORY ONLY
            ( file_existsMod .AND. (file_sizeMod /= 0) )     .AND. &   !THE INPUT MODEL FILE INPUTMODELFILENAME EXISTS
            ( file_existsPath .AND. (file_sizePath /= 0) )   .AND. &   !THE INPUT FILE INPUTMODELFILENAME EXISTS
            ( file_existsBlock .AND. (file_sizeBlock /= 0) ) .AND. &   !THE INPUT FILE INPUTBLOCKFILENAME EXISTS
            ( file_existsSolver .AND. (file_sizeSolver /= 0) ) )then   !THE SOLVER FILE INPUTSOLVERPATHNAME EXISTS

            !╔═══════════════════════╗
            !║CREATE SOLVERCONTROLLER║
            !╚═══════════════════════╝
            allocate( controller, STAT = status, SOURCE = DDDMS_SolverController( HPC_Arch ) );
            if (status /= 0) then
                print*, "Failed allocation of DDM controller!";
                print*, "Errore code: ", status;
                pause;
                stop;
            end if  !CLOSE IF STATEMENT ON ALLOCATE STATUS FOR CONTROLLER OBJECT.

            !╔════════════════════════════════════════════════════════════════╗
            !║OBTAIN DATA LOADER (LOAD FROM FILE) FROM SOLVERCONTROLLER OBJECT║
            !╚════════════════════════════════════════════════════════════════╝
            allocate( loader, STAT = status, SOURCE = controller%getDataLoader( inputPathFileName ) );
            if (status /= 0) then
                print*, "Failed allocation of DDM loader!";
                print*, "Errore code: ", status;
                pause;
                stop;
            end if  !CLOSE IF STATEMENT ON ALLOCATE STATUS FOR LOADER OBJECT.

            !╔═════════════════════════════╗
            !║LOOP OVER BATCH FILE COMMANDS║
            !╚═════════════════════════════╝ 
            do k = 1, size( batchFileCommands, 1)
                
                currBatchCommand = 0;
                
                call To_upper( batchFileCommands(k) ); 

                currBatchCommand = currBatchCommand + merge(1, 0, index( batchFileCommands(k), "BEASY"      ) /= 0);
                currBatchCommand = currBatchCommand + merge(2, 0, index( batchFileCommands(k), "CALC_LSC"   ) /= 0);
                currBatchCommand = currBatchCommand + merge(3, 0, index( batchFileCommands(k), "ASSEMB_GSC" ) /= 0);
                currBatchCommand = currBatchCommand + merge(4, 0, index( batchFileCommands(k), "CALC_IS"    ) /= 0);
                currBatchCommand = currBatchCommand + merge(5, 0, index( batchFileCommands(k), "CALC_BS"    ) /= 0);
                currBatchCommand = currBatchCommand + merge(6, 0, index( batchFileCommands(k), "RES_VEC"    ) /= 0);
                
                commandLine      = trim( '"'//trim( inputSolverPathName(:) )//'"'//" -rootname "//'"'//trim( inputModelFileName( 1:index(inputModelFileName, '.dat') - 1) )//'"' ); 
                
                select case ( currBatchCommand )
                    case ( 1 )
                        write(OUTPUT_UNIT, '("BEASY Solver is being called............................: ")', ADVANCE = "NO");
                        flush OUTPUT_UNIT;
                        call cpu_time(start_time);
                        call execute_command_line ( commandLine, wait = .true. ); !SYNCHRONOUS CALL TO BEASY SOLVER.
                        call cpu_time(stop_time)
                        write(OUTPUT_UNIT, '("(Elapsed Time : ")', ADVANCE = "NO");
                        write(OUTPUT_UNIT, '(E12.4)', ADVANCE = "NO") (stop_time - start_time);
                        write(OUTPUT_UNIT, '("-[s]) ")', ADVANCE = "NO");

                        write(OUTPUT_UNIT, '("DONE")');
                        write(OUTPUT_UNIT, *);
                    
                    case( 2 )
                        write(OUTPUT_UNIT, '("Local Schur Complement calculation is being performed...: ")', ADVANCE = "NO");
                        flush OUTPUT_UNIT;
                        
                        !╔════════════════════════════════════════════════════════════════════════════════════════╗
                        !║CHECK DATA IN FOLDER PATHNAME PASSED IN INPUT USING CHECK PROCEDURE OF THE LOADER OBJECT║
                        !╚════════════════════════════════════════════════════════════════════════════════════════╝
                        if( loader%CheckData() ) then
                        
                            !┌──────────────────────────────────────┐
                            !│CREATE INPUT PARAMETERS DATA CONTAINER│
                            !└──────────────────────────────────────┘
                            allocate( inputParamsData, STAT = status, SOURCE = DDDMS_InputParams() );
                            if (status /= 0) then
                                print*, "Failed allocation of inputParamsData data structure!";
                                print*, "Errore code: ", status;
                                pause;
                                stop;
                            end if  !CLOSE IF STATEMENT ON ALLOCATE STATUS FOR INPUTPARAMSDATA OBJECT.

                        
                            !┌────────────────┐
                            !│LOAD INPUTPARAMS│
                            !└────────────────┘
                            call inputParamsData%loadInputParams( inputPathFileName, inputBlockFileName );
                        
                            !┌─────────────────────┐
                            !│CREATE DATA CONTAINER│
                            !└─────────────────────┘
                            allocate( dataContainer, STAT = status, SOURCE = DDDMS_DataContainer() );
                            if (status /= 0) then
                                print*, "Failed allocation of DDM controller!";
                                print*, "Errore code: ", status;
                                pause;
                                stop;
                            end if  !CLOSE IF STATEMENT ON ALLOCATE STATUS FOR DATACONTAINER OBJECT.

                        
                            !┌──────────────────────────────────────────────────────────────────────────────┐
                            !│LOAD DATA FROM A SPECIFIC FOLDER USING LOADDATA PROCEDURE OF THE LOADER OBJECT│
                            !└──────────────────────────────────────────────────────────────────────────────┘                            
                            call loader%LoadData( dataContainer, inputParamsData );     ! APB: Polymorphism with type bound procedure
 
                            !╔═════════════════════════════════════════════════════════════════════════════════════════════════════════╗
                            !║OBTAIN LOCAL SCHUR COMPLEMENT CALCULATOR OBJECT (SHARED MEMORY ARCHITECTURE) FROM SOLVERCONTROLLER OBJECT║
                            !╚═════════════════════════════════════════════════════════════════════════════════════════════════════════╝
                            allocate( LocSchurComp_Calculator, STAT = status, SOURCE = controller%getLocalSchurComplementCalculator( HPC_Arch ) );
                            if (status /= 0) then
                                print*, "Failed allocation of DDM loader!";
                                print*, "Errore code: ", status;
                                pause;
                                stop;
                            end if  !CLOSE IF STATEMENT ON ALLOCATE STATUS FOR LOCSCHURCOMP_CALCULATOR OBJECT.

                            !┌────────────────────────────────────────────────────────────────────┐
                            !│CALCULATE LOCAL SCHUR COMPLEMENT FOR ALL ZONES INVOLVED IN THE MODEL│
                            !└────────────────────────────────────────────────────────────────────┘
                            call cpu_time(start_time);
                            call LocSchurComp_Calculator%calculate( dataContainer, inputParamsData );
                            call cpu_time(stop_time)
                            write(OUTPUT_UNIT, '("(Elapsed Time : ")', ADVANCE = "NO");
                            write(OUTPUT_UNIT, '(E12.4)', ADVANCE = "NO") (stop_time - start_time);
                            write(OUTPUT_UNIT, '("-[s]) ")', ADVANCE = "NO");
                            
                            write(OUTPUT_UNIT, '("DONE")');
                            write(OUTPUT_UNIT, *);
                            
                        else
                            print*, "Check function returned an error.";
                            print*, "Please, check input files path name folder ";
                            pause;
                            stop;
                        endif !END OF IF STATEMENT CHECKING DATA
                        
                    case( 3 )
                        write(OUTPUT_UNIT, '("Global Schur Complement assembling is being performed...: ")', ADVANCE = "NO");
                        flush OUTPUT_UNIT;

                        !╔═════════════════════════════════════════════════════════════════════╗
                        !║OBTAIN GLOBAL SCHUR COMPLEMENT ASSEMBLER FROM SOLVERCONTROLLER OBJECT║
                        !╚═════════════════════════════════════════════════════════════════════╝
                        allocate( GloSchurComp_Assembler, STAT = status, SOURCE = controller%getGlobalSchurComplementAssembler() );
                        if (status /= 0) then
                            print*, "Failed allocation of DDM assembler!";
                            print*, "Errore code: ", status;
                            pause;
                            stop;
                        end if  !CLOSE IF STATEMENT ON ALLOCATE STATUS FOR LOADER OBJECT.

                        !╔═════════════════════════════════════════════════════════════════════════════════════════╗
                        !║CALL SUBROUTINE "ASSEMBLY" TO ASSEMBLY ALL CONTRIBUTES PROVIDED BY ALL ZONES IN THE MODEL║
                        !╚═════════════════════════════════════════════════════════════════════════════════════════╝
                        call cpu_time(start_time);
                        call GloSchurComp_Assembler%Assembly( dataContainer,  inputParamsData);
                        call cpu_time(stop_time)
                        write(OUTPUT_UNIT, '("(Elapsed Time : ")', ADVANCE = "NO");
                        write(OUTPUT_UNIT, '(E12.4)', ADVANCE = "NO") (stop_time - start_time);
                        write(OUTPUT_UNIT, '("-[s]) ")', ADVANCE = "NO");
                        
                        write(OUTPUT_UNIT, '("DONE")');
                        write(OUTPUT_UNIT, *);
                        
                    case( 4 )    
                        write(OUTPUT_UNIT, '("Interface solution is being calculated..................: ")', ADVANCE = "NO");
                        flush OUTPUT_UNIT;
                        
                        !╔═════════════════════════════════════════════════════════════════╗
                        !║OBTAIN INTERFACE SOLUTION CALCULATOR FROM SOLVERCONTROLLER OBJECT║
                        !╚═════════════════════════════════════════════════════════════════╝
                        allocate( InterSol_Calculator, STAT = status, SOURCE = controller%getInterfaceSolutionCalculator(HPC_Arch) );
                        if (status /= 0) then
                            print*, "Failed allocation of DDM interface solution calculator!";
                            print*, "Errore code: ", status;
                            pause;
                            stop;
                        end if  !CLOSE IF STATEMENT ON ALLOCATE STATUS FOR LOADER OBJECT.

                        !╔═════════════════════════════════════════════════════════════════════════════════════╗
                        !║CALL SUBROUTINE "CALCULATE" TO PERFORM CALCULATION OF INTERFACE SOLUTION ON INTERFACE║
                        !╚═════════════════════════════════════════════════════════════════════════════════════╝                        
                        call cpu_time(start_time);
                        call InterSol_Calculator%calculate( dataContainer);
                        call cpu_time(stop_time)
                        write(OUTPUT_UNIT, '("(Elapsed Time : ")', ADVANCE = "NO");
                        write(OUTPUT_UNIT, '(E12.4)', ADVANCE = "NO") (stop_time - start_time);
                        write(OUTPUT_UNIT, '("-[s]) ")', ADVANCE = "NO");
                            
                        write(OUTPUT_UNIT, '("DONE")');
                        write(OUTPUT_UNIT, *);
                        
                    case( 5 )    
                        write(OUTPUT_UNIT, '("Boundary solution is being calculated...................: ")', ADVANCE = "NO");
                        flush OUTPUT_UNIT;

                        !╔════════════════════════════════════════════════════════════════╗
                        !║OBTAIN BOUNDARY SOLUTION CALCULATOR FROM SOLVERCONTROLLER OBJECT║
                        !╚════════════════════════════════════════════════════════════════╝
                        allocate( BoundSol_Calculator, STAT = status, SOURCE = controller%getBoundarySolutionCalculator(HPC_Arch) );
                        if (status /= 0) then
                            print*, "Failed allocation of DDM boundary solution calculator!";
                            print*, "Errore code: ", status;
                            pause;
                            stop;
                        end if  !CLOSE IF STATEMENT ON ALLOCATE STATUS FOR LOADER OBJECT.
                        
                        !╔═══════════════════════════════════════════════════════════════════════════════════════════════════╗
                        !║CALL SUBROUTINE "CALCULATE" TO PERFORM CALCULATION BOUNDARY SOLUTION ON ZONES INVOLVED IN THE MODEL║
                        !╚═══════════════════════════════════════════════════════════════════════════════════════════════════╝                        
                        allocate(zonesBounSol, SOURCE = Utility_ZoneBoundarySolution( batchFileCommands(k), inputParamsData ) );
                                              
                        call cpu_time(start_time);
                        call BoundSol_Calculator%calculate( dataContainer, zonesBounSol, inputParamsData );
                        call cpu_time(stop_time)
                        write(OUTPUT_UNIT, '("(Elapsed Time : ")', ADVANCE = "NO");
                        write(OUTPUT_UNIT, '(E12.4)', ADVANCE = "NO") (stop_time - start_time);
                        write(OUTPUT_UNIT, '("-[s]) ")', ADVANCE = "NO");
                            
                        write(OUTPUT_UNIT, '("DONE")');
                        write(OUTPUT_UNIT, *);
                    case( 6 )    
                        write(OUTPUT_UNIT,*)'Creating results vector';
                        flush OUTPUT_UNIT;
                        nzones=size(dataContainer%ZonesData);
                        allocate( ResVec, STAT = status, SOURCE = DDDMS_getResultsVector() );
!                        allocate( ResVec%ZonalResults(nzones), STAT = status);
                        if (status.gt.0) then
                            write(*,*)'Allocation failed for results vector object!....'
                            stop;
                        endif
                        call ResVec%getResultsVector(dataContainer);
                        
                    case default
                        print *, "Entered command was not recognised.";
                        print *, "Check input command.";
                        
                end select
            end do ! END OF LOOP OVER INPUT BATCH FILE COMMANDS         
            
        else
            print*, "Check HPC hardware architecture selected.";
            pause;
            stop;
        endif !CLOSE IF STATEMENT ON CHECKING HPC HARDWARE ARCHITECTURE.
        

        pause;
        stop;
    end program DirectDDMMain

