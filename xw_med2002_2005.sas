/* xwip2002_2005.sas
   cross-walk the ip files for 2002-2005 to add bene_id
   put in Xwalk2002_2005 data directory
*/
options ls=125 ps=50 nocenter replace compress=yes mprint FILELOCKS=NONE;

%include "/disk/agedisk3/medicare.work/goldman-DUA25731/PROGRAMS/setup.inc";
%include "&maclib.xwalk0205.mac";

%partABlib(types=med);
libname xwalk "&datalib.&xwalk_data";
libname clean "/disk/agedisk3/medicare.work/goldman-DUA25731/DATA/Clean_Data/MedPAR";

%let contentsdir=&doclib.&xwalk_data./Contents/;

%xwyr(med,2002,2005,inlib=med,outlib=clean,renfn=N,contlist=Y,contdir=&contentsdir);
%xwyr(med,2002,2005,inlib=med,outlib=clean,renfn=N,contlist=Y,contdir=&contentsdir);
