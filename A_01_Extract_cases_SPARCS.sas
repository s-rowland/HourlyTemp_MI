
libname sparcs 'N:\CSID\SPARCS\datasets\';
/***************************/
/***** Extract MI Cases*****/
/***************************/
%macro ExtractCase_mi(YYYY, PP); 
data df_mi;	
	set sparcs.sparcs_&PP.&YYYY;
	if DXA           in ('41000', '41001', '41002', '41010', '41011', '41012', '41020', '41021', '41022', '41030', '41031', '41032',
						 '41040', '41041', '41042', '41050', '41051', '41052', '41060', '41061', '41062', '41070', '41071', '41072', 
						 '41080', '41081', '41082', '41090', '41091', '41092' ) 
	then miA_012= 1; else miA_012=0;

	if DX01         in ('41000', '41001', '41002', '41010', '41011', '41012', '41020', '41021', '41022', '41030', '41031', '41032',
						 '41040', '41041', '41042', '41050', '41051', '41052', '41060', '41061', '41062', '41070', '41071', '41072', 
						 '41080', '41081', '41082', '41090', '41091', '41092' ) 
	then mi1_012= 1; else mi1_012=0;
	
	if DX02         in ('41000', '41001', '41002', '41010', '41011', '41012', '41020', '41021', '41022', '41030', '41031', '41032',
						 '41040', '41041', '41042', '41050', '41051', '41052', '41060', '41061', '41062', '41070', '41071', '41072', 
						 '41080', '41081', '41082', '41090', '41091', '41092' ) 
	then mi2_012= 1; else mi2_012=0;

	if DX03          in ('41000', '41001', '41002', '41010', '41011', '41012', '41020', '41021', '41022', '41030', '41031', '41032',
						 '41040', '41041', '41042', '41050', '41051', '41052', '41060', '41061', '41062', '41070', '41071', '41072', 
						 '41080', '41081', '41082', '41090', '41091', '41092' )  
	then mi3_012= 1; else mi3_012=0;

	if DX04          in ('41000', '41001', '41002', '41010', '41011', '41012', '41020', '41021', '41022', '41030', '41031', '41032',
						 '41040', '41041', '41042', '41050', '41051', '41052', '41060', '41061', '41062', '41070', '41071', '41072', 
						 '41080', '41081', '41082', '41090', '41091', '41092' ) 
	then mi4_012= 1; else mi4_012=0;

	if DX05           in ('41000', '41001', '41002', '41010', '41011', '41012', '41020', '41021', '41022', '41030', '41031', '41032',
						 '41040', '41041', '41042', '41050', '41051', '41052', '41060', '41061', '41062', '41070', '41071', '41072', 
						 '41080', '41081', '41082', '41090', '41091', '41092' )  
	then mi5_012= 1; else mi5_012=0;

	if DX06           in ('41000', '41001', '41002', '41010', '41011', '41012', '41020', '41021', '41022', '41030', '41031', '41032',
						 '41040', '41041', '41042', '41050', '41051', '41052', '41060', '41061', '41062', '41070', '41071', '41072', 
						 '41080', '41081', '41082', '41090', '41091', '41092' ) 
	then mi6_012= 1; else mi6_012=0;

	if DX07           in ('41000', '41001', '41002', '41010', '41011', '41012', '41020', '41021', '41022', '41030', '41031', '41032',
						 '41040', '41041', '41042', '41050', '41051', '41052', '41060', '41061', '41062', '41070', '41071', '41072', 
						 '41080', '41081', '41082', '41090', '41091', '41092' ) 
	then mi7_012= 1; else mi7_012=0;

	if DX08          in ('41000', '41001', '41002', '41010', '41011', '41012', '41020', '41021', '41022', '41030', '41031', '41032',
						 '41040', '41041', '41042', '41050', '41051', '41052', '41060', '41061', '41062', '41070', '41071', '41072', 
						 '41080', '41081', '41082', '41090', '41091', '41092' ) 
	then mi8_012= 1; else mi8_012=0;

	if DX09          in ('41000', '41001', '41002', '41010', '41011', '41012', '41020', '41021', '41022', '41030', '41031', '41032',
						 '41040', '41041', '41042', '41050', '41051', '41052', '41060', '41061', '41062', '41070', '41071', '41072', 
						 '41080', '41081', '41082', '41090', '41091', '41092' ) 
	then mi9_012= 1; else mi9_012=0;

	if DX10           in ('41000', '41001', '41002', '41010', '41011', '41012', '41020', '41021', '41022', '41030', '41031', '41032',
						 '41040', '41041', '41042', '41050', '41051', '41052', '41060', '41061', '41062', '41070', '41071', '41072', 
						 '41080', '41081', '41082', '41090', '41091', '41092' ) 
	then mi10_012= 1; else mi10_012=0;

	if DX11           in ('41000', '41001', '41002', '41010', '41011', '41012', '41020', '41021', '41022', '41030', '41031', '41032',
						 '41040', '41041', '41042', '41050', '41051', '41052', '41060', '41061', '41062', '41070', '41071', '41072', 
						 '41080', '41081', '41082', '41090', '41091', '41092' ) 
	then mi11_012= 1; else mi11_012=0;

	if DX12          in ('41000', '41001', '41002', '41010', '41011', '41012', '41020', '41021', '41022', '41030', '41031', '41032',
						 '41040', '41041', '41042', '41050', '41051', '41052', '41060', '41061', '41062', '41070', '41071', '41072', 
						 '41080', '41081', '41082', '41090', '41091', '41092' ) 
	then mi12_012= 1; else mi12_012=0;
if miA_012=1 
then MI_A_012=1; else MI_A_012=0;

	if miA_012=1 OR mi1_012=1  OR mi2_012=1 OR mi3_012=1 OR  mi4_012=1  OR mi5_012=1 OR mi6_012=1 OR
       mi7_012=1  OR mi8_012=1 OR mi9_012=1 OR mi10_012=1 OR mi11_012=1 OR mi12_012=1
 
then MI_012_prob=1; else MI_012_prob=0;
if MI_012_prob=0 then delete;

	if miA_012=1 OR mi1_012=1  OR mi2_012=1 OR mi3_012=1 
then MI_012_prim=1; else MI_012_prim=0;
run;

data df_mi2;	
	set df_mi;
	if DXA           in ('41000', '41001',  '41010', '41011',  '41020', '41021',  '41030', '41031', 
						  '41040', '41041',  '41050', '41051',  '41060', '41061',  '41070', '41071',  
						  '41080', '41081',  '41090', '41091' ) 
	then miA_01= 1; else miA_01=0;

	if DX01          in ('41000', '41001',  '41010', '41011',  '41020', '41021',  '41030', '41031', 
						  '41040', '41041',  '41050', '41051',  '41060', '41061',  '41070', '41071',  
						  '41080', '41081',  '41090', '41091' ) 
	then mi1_01= 1; else mi1_01=0;
	
	if DX02          in ('41000', '41001',  '41010', '41011',  '41020', '41021',  '41030', '41031', 
						  '41040', '41041',  '41050', '41051',  '41060', '41061',  '41070', '41071',  
						  '41080', '41081',  '41090', '41091' ) 
	then mi2_01= 1; else mi2_01=0;

	if DX03        in ('41000', '41001',  '41010', '41011',  '41020', '41021',  '41030', '41031', 
						  '41040', '41041',  '41050', '41051',  '41060', '41061',  '41070', '41071',  
						  '41080', '41081',  '41090', '41091' ) 
	then mi3_01= 1; else mi3_01=0;

	if DX04          in ('41000', '41001',  '41010', '41011',  '41020', '41021',  '41030', '41031', 
						  '41040', '41041',  '41050', '41051',  '41060', '41061',  '41070', '41071',  
						  '41080', '41081',  '41090', '41091' ) 
	then mi4_01= 1; else mi4_01=0;

	if DX05           in ('41000', '41001',  '41010', '41011',  '41020', '41021',  '41030', '41031', 
						  '41040', '41041',  '41050', '41051',  '41060', '41061',  '41070', '41071',  
						  '41080', '41081',  '41090', '41091' ) 
	then mi5_01= 1; else mi5_01=0;

	if DX06           in ('41000', '41001',  '41010', '41011',  '41020', '41021',  '41030', '41031', 
						  '41040', '41041',  '41050', '41051',  '41060', '41061',  '41070', '41071',  
						  '41080', '41081',  '41090', '41091' ) 
	then mi6_01= 1; else mi6_01=0;

	if DX07            in ('41000', '41001',  '41010', '41011',  '41020', '41021',  '41030', '41031', 
						  '41040', '41041',  '41050', '41051',  '41060', '41061',  '41070', '41071',  
						  '41080', '41081',  '41090', '41091' ) 
	then mi7_01= 1; else mi7_01=0;

	if DX08          in ('41000', '41001',  '41010', '41011',  '41020', '41021',  '41030', '41031', 
						  '41040', '41041',  '41050', '41051',  '41060', '41061',  '41070', '41071',  
						  '41080', '41081',  '41090', '41091' ) 
	then mi8_01= 1; else mi8_01=0;

	if DX09           in ('41000', '41001',  '41010', '41011',  '41020', '41021',  '41030', '41031', 
						  '41040', '41041',  '41050', '41051',  '41060', '41061',  '41070', '41071',  
						  '41080', '41081',  '41090', '41091' ) 
	then mi9_01= 1; else mi9_01=0;

	if DX10           in ('41000', '41001',  '41010', '41011',  '41020', '41021',  '41030', '41031', 
						  '41040', '41041',  '41050', '41051',  '41060', '41061',  '41070', '41071',  
						  '41080', '41081',  '41090', '41091' ) 
	then mi10_01= 1; else mi10_01=0;

	if DX11           in ('41000', '41001',  '41010', '41011',  '41020', '41021',  '41030', '41031', 
						  '41040', '41041',  '41050', '41051',  '41060', '41061',  '41070', '41071',  
						  '41080', '41081',  '41090', '41091' ) 
	then mi11_01= 1; else mi11_01=0;

	if DX12          in ('41000', '41001',  '41010', '41011',  '41020', '41021',  '41030', '41031', 
						  '41040', '41041',  '41050', '41051',  '41060', '41061',  '41070', '41071',  
						  '41080', '41081',  '41090', '41091' ) 
	then mi12_01= 1; else mi12_01=0;
if miA_01=1 
then MI_A_01=1; else MI_A_01=0;

	if miA_01=1 OR mi1_01=1 OR mi2_01=1 OR mi3_01=1  OR mi4_01=1 OR mi5_01=1 OR mi6_01=1 OR
       mi7_01=1 OR mi8_01=1 OR mi9_01=1 OR mi10_01=1 OR mi11_01=1 OR mi12_01=1
 
then MI_01_prob=1; else MI_01_prob=0;

	if miA_01=1 OR mi1_01=1  OR mi2_01=1 OR mi3_01=1 
then MI_01_prim=1; else MI_01_prim=0;
run;

data df_mi3;	
	set df_mi2;
keep ADMDT ADMHR AGE DOB RACE SEX UPID ETHNIC ZIP
TPADM
MI_A_01 MI_A_012 
MI_012_prob MI_012_prim
MI_01_prob MI_01_prim
DXA DX01 DX02 DX03 DX04 DX05
Source1 Source2 Source3 Source4 Source5
; 
run;

proc export data = df_mi3
outfile = "H:\Temp_CVD_Analysis\Data\Raw_Data\Outcome_Data\raw_&YYYY._&PP._mi.csv"
DBMS =csv;
run;
%mend ExtractCase;

%ExtractCase_mi(95, ip);
%ExtractCase_mi(96, ip);
%ExtractCase_mi(97, ip);
%ExtractCase_mi(98, ip);
%ExtractCase_mi(99, ip);


%ExtractCase_mi(00, ip);
%ExtractCase_mi(01, ip);
%ExtractCase_mi(02, ip);
%ExtractCase_mi(03, ip);
%ExtractCase_mi(04, ip);
%ExtractCase_mi(05, ip);
%ExtractCase_mi(06, ip);
%ExtractCase_mi(07, ip);
%ExtractCase_mi(08, ip);
%ExtractCase_mi(09, ip);
%ExtractCase_mi(10, ip);
%ExtractCase_mi(11, ip);
%ExtractCase_mi(12, ip);
%ExtractCase_mi(13, ip);
%ExtractCase_mi(14, ip);
%ExtractCase_mi(15, ip);


%ExtractCase_mi(95, op);
%ExtractCase_mi(96, op);
%ExtractCase_mi(97, op);
%ExtractCase_mi(98, op);
%ExtractCase_mi(99, op);

%ExtractCase_mi(00, op);
%ExtractCase_mi(01, op);
%ExtractCase_mi(02, op);
%ExtractCase_mi(03, op);
%ExtractCase_mi(04, op);
%ExtractCase_mi(05, op);
%ExtractCase_mi(06, op);
%ExtractCase_mi(07, op);
%ExtractCase_mi(08, op);
%ExtractCase_mi(09, op);
%ExtractCase_mi(10, op);
%ExtractCase_mi(11, op);
%ExtractCase_mi(12, op);
%ExtractCase_mi(13, op);
%ExtractCase_mi(14, op);
%ExtractCase_mi(15, op);


/***************************/
/***** Extract Stroke Cases*****/
/***************************/
%macro ExtractCase_cva(YYYY, PP); 

data df_stroke;	
	set sparcs.sparcs_&PP.&YYYY;
	if DXA           in ('43301', '43311', '43321' , '43331', '43381', '43391'
                        '43401', '43411', '43491', '436', '436') 
	then isc_strokeA= 1; else isc_strokeA=0;

	if DX01          in ('43301', '43311', '43321' , '43331', '43381', '43391'
                        '43401', '43411', '43491', '436', '436') 
	then isc_stroke1= 1; else isc_stroke1=0;

	if DX02            in ('43301', '43311', '43321' , '43331', '43381', '43391'
                        '43401', '43411', '43491', '436', '436') 
	then isc_stroke2= 1; else isc_stroke2=0;

	if DX03           in ('43301', '43311', '43321' , '43331', '43381', '43391'
                        '43401', '43411', '43491', '436', '436') 
	then isc_stroke3= 1; else isc_stroke3=0;

	if DX04            in ('43301', '43311', '43321' , '43331', '43381', '43391'
                        '43401', '43411', '43491', '436', '436') 
	then isc_stroke4= 1; else isc_stroke4=0;

	if DX05             in ('43301', '43311', '43321' , '43331', '43381', '43391'
                        '43401', '43411', '43491', '436', '436')  
	then isc_stroke5= 1; else isc_stroke5=0;

	if DX06            in ('43301', '43311', '43321' , '43331', '43381', '43391'
                        '43401', '43411', '43491', '436', '436')  
	then isc_stroke6= 1; else isc_stroke6=0;

	if DX07            in ('43301', '43311', '43321' , '43331', '43381', '43391'
                        '43401', '43411', '43491', '436', '436') 
	then isc_stroke7= 1; else isc_stroke7=0;

	if DX08            in ('43301', '43311', '43321' , '43331', '43381', '43391'
                        '43401', '43411', '43491', '436', '436') 
	then isc_stroke8= 1; else isc_stroke8=0;

	if DX09          in ('43301', '43311', '43321' , '43331', '43381', '43391'
                        '43401', '43411', '43491', '436', '436') 
	then isc_stroke9= 1; else isc_stroke9=0;

	if DX10            in ('43301', '43311', '43321' , '43331', '43381', '43391'
                        '43401', '43411', '43491', '436', '436') 
	then isc_stroke10= 1; else isc_stroke10=0;

	if DX11            in ('43301', '43311', '43321' , '43331', '43381', '43391'
                        '43401', '43411', '43491','436', '436') 
	then isc_stroke11= 1; else isc_stroke11=0;

	if DX12           in ('43301', '43311', '43321' , '43331', '43381', '43391'
                        '43401', '43411', '43491', '436', '436') 
	then isc_stroke12= 1; else isc_stroke12=0;

	if isch_stroke1=1 OR isc_stroke2=1 OR isc_stroke3=1 OR isc_stroke4=1 OR
   isc_stroke5=1 OR isc_stroke6=1 OR isc_stroke7=1 OR isc_stroke8=1 OR
   isc_stroke9=1 OR isc_stroke10=1 OR isc_stroke11=1 OR isc_stroke12=1
 
then isc_stroke=1; else isc_stroke=0;

if DXA  in ('430' ,'431', '4310', '43100' , '432', '4320','43200', '4321' ,'4329') then hem_strokeA= 1; else hem_strokeA=0;
if DX01  in ('430' ,'431', '4310', '43100' , '432', '4320','43200', '4321' ,'4329') then hem_stroke1= 1; else hem_stroke1=0;
if DX02  in ('430' ,'431', '4310', '43100' , '432', '4320','43200', '4321' ,'4329') then hem_stroke2= 1; else hem_stroke2=0;
if DX03  in ('430' ,'431', '4310', '43100' , '432', '4320','43200', '4321' ,'4329') then hem_stroke3= 1; else hem_stroke3=0;
if DX04  in ('430' ,'431', '4310', '43100' , '432', '4320','43200', '4321' ,'4329') then hem_stroke4= 1; else hem_stroke4=0;
if DX05  in ('430' ,'431', '4310', '43100' , '432', '4320','43200', '4321' ,'4329') then hem_stroke5= 1; else hem_stroke5=0;
if DX06  in ('430' ,'431', '4310', '43100' , '432', '4320','43200', '4321' ,'4329') then hem_stroke6= 1; else hem_stroke6=0;
if DX07  in ('430' ,'431', '4310', '43100' , '432', '4320','43200', '4321' ,'4329') then hem_stroke7= 1; else hem_stroke7=0;
if DX08  in ('430' ,'431', '4310', '43100' , '432', '4320','43200', '4321' ,'4329') then hem_stroke8= 1; else hem_stroke8=0;
if DX09  in ('430' ,'431', '4310', '43100' , '432', '4320','43200', '4321' ,'4329') then hem_stroke9= 1; else hem_stroke9=0;
if DX10  in ('430' ,'431', '4310', '43100' , '432', '4320','43200', '4321' ,'4329') then hem_stroke10= 1; else hem_stroke10=0;
if DX11  in ('430' ,'431', '4310', '43100' , '432', '4320','43200', '4321' ,'4329') then hem_stroke11= 1; else hem_stroke11=0;
if DX12  in ('430' ,'431', '4310', '43100' , '432', '4320','43200', '4321' ,'4329') then hem_stroke12= 1; else hem_stroke12=0;

if hem_strokeA = 1 or hem_stroke1 = 1 or hem_stroke2 = 1 or hem_stroke3 = 1 or hem_stroke4 =1 or hem_stroke5 = 1 or hem_stroke6 = 1 or hem_stroke7 = 1 or 
hem_stroke8 = 1 or hem_stroke9 = 1 or hem_stroke10 = 1 or hem_stroke11 = 1 or hem_stroke12 = 1 then hem_stroke = 1;
else hem_stroke = 0;

if isc_stroke=1 or hem_stroke=1 then all_stroke=1; else all_stroke=0;

	if isc_stroke=0 and hem_stroke=0  then delete;





	if isc_strokeA=1 OR isc_stroke1=1 OR isc_stroke2=1 OR isc_stroke3=1 OR isc_stroke4=1 OR
   isc_stroke5=1 OR isc_stroke6=1 OR isc_stroke7=1 OR isc_stroke8=1 OR
   isc_stroke9=1 OR isc_stroke10=1 OR isc_stroke11=1 OR isc_stroke12=1
then isc_stroke_prob=1; else isc_stroke_prob=0;

	if isc_strokeA=1 OR isc_stroke1=1 OR isc_stroke2=1 OR isc_stroke3=1
then isc_stroke_prim=1; else isc_stroke_prim=0;

	if hem_strokeA=1 OR hem_stroke1=1 OR hem_stroke2=1 OR hem_stroke3=1 OR hem_stroke4=1 OR
   hem_stroke5=1 OR hem_stroke6=1 OR hem_stroke7=1 OR hem_stroke8=1 OR
   hem_stroke9=1 OR hem_stroke10=1 OR hem_stroke11=1 OR hem_stroke12=1
then hem_stroke_prob=1; else hem_stroke_prob=0;

	if hem_strokeA=1 OR hem_stroke1=1 OR hem_stroke2=1 OR hem_stroke3=1
then hem_stroke_prim=1; else hem_stroke_prim=0;

if isc_stroke_prob=1 OR hem_stroke_prob=1
then all_stroke_prob=1; 

if isc_stroke_prim=1 OR hem_stroke_prim=1
then all_stroke_prim=1; 

run;


data df_stroke2;	
	set df_stroke;
keep ADMDT ADMHR AGE DOB RACE SEX UPID ETHNIC ZIP
isc_stroke_prob isc_stroke_prim hem_stroke_prim hem_stroke_prob all_stroke_prob all_stroke_prim 
isc_strokeA hem_strokeA
dxa dx01 dx02 dx03 dx04 dx05 pr01 pr02 pr03 pr04 pr05;
run;

proc export data = df_stroke2
outfile = "H:\Temp_CVD_Analysis\Data\Raw_Data\Outcome_Data\raw_&YYYY._&PP._stroke.csv"
DBMS =csv;
run;
%mend ExtractCase_cva;

%ExtractCase_cva(95, ip);
%ExtractCase_cva(96, ip);
%ExtractCase_cva(97, ip);
%ExtractCase_cva(98, ip);
%ExtractCase_cva(99, ip);

%ExtractCase_cva(00, ip);
%ExtractCase_cva(01, ip);
%ExtractCase_cva(02, ip);
%ExtractCase_cva(03, ip);
%ExtractCase_cva(04, ip);
%ExtractCase_cva(05, ip);
%ExtractCase_cva(06, ip);
%ExtractCase_cva(07, ip);
%ExtractCase_cva(08, ip);
%ExtractCase_cva(09, ip);
%ExtractCase_cva(10, ip);
%ExtractCase_cva(11, ip);
%ExtractCase_cva(12, ip);
%ExtractCase_cva(13, ip);
%ExtractCase_cva(14, ip);
%ExtractCase_cva(15, ip);

%ExtractCase_cva(95, op);
%ExtractCase_cva(96, op);
%ExtractCase_cva(97, op);
%ExtractCase_cva(98, op);
%ExtractCase_cva(99, op);

%ExtractCase_cva(00, op);
%ExtractCase_cva(01, op);
%ExtractCase_cva(02, op);
%ExtractCase_cva(03, op);
%ExtractCase_cva(04, op);
%ExtractCase_cva(05, op);
%ExtractCase_cva(06, op);
%ExtractCase_cva(07, op);
%ExtractCase_cva(08, op);
%ExtractCase_cva(09, op);
%ExtractCase_cva(10, op);
%ExtractCase_cva(11, op);
%ExtractCase_cva(12, op);
%ExtractCase_cva(13, op);
%ExtractCase_cva(14, op);
%ExtractCase_cva(15, op);


