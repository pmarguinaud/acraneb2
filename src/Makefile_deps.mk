# to generate this file, run ../util/finddeps.sh > Makefile_deps.mk in the src/ directory
ac_cloud_model2.o:  parkind1.o yomcst.o yomhook_dummy.o yomphy.o yomphy3.o ac_cloud_model2.F90
	$(FC) -c $(FCOPTS) ac_cloud_model2.F90
acraneb2_1.o:  model_physics_mf_mod.o parkind1.o yomhook_dummy.o yomcst.o yomrip.o yoerdi.o acraneb2_1.F90
	$(FC) -c $(FCOPTS) acraneb2_1.F90
acraneb2_2.o:  model_physics_mf_mod.o parkind1.o yomhook_dummy.o yomcst.o yomrip.o yoerdi.o acraneb2_2.F90
	$(FC) -c $(FCOPTS) acraneb2_2.F90
acraneb_coefs.o:  parkind1.o yomhook_dummy.o yomphy3.o acraneb_coefs.F90
	$(FC) -c $(FCOPTS) acraneb_coefs.F90
acraneb_coeft.o:  parkind1.o yomhook_dummy.o acraneb_coeft.F90
	$(FC) -c $(FCOPTS) acraneb_coeft.F90
acraneb_solvs.o:  parkind1.o yomphy.o yomhook_dummy.o acraneb_solvs.F90
	$(FC) -c $(FCOPTS) acraneb_solvs.F90
acraneb_solvt3.o:  parkind1.o yomhook_dummy.o yomphy.o acraneb_solvt3.F90
	$(FC) -c $(FCOPTS) acraneb_solvt3.F90
acraneb_solvt.o:  parkind1.o yomhook_dummy.o yomphy.o acraneb_solvt.F90
	$(FC) -c $(FCOPTS) acraneb_solvt.F90
acraneb_transs.o:  parkind1.o yomhook_dummy.o yomcst.o yomphy.o yomphy3.o acraneb_transs.F90
	$(FC) -c $(FCOPTS) acraneb_transs.F90
acraneb_transt.o:  parkind1.o yomhook_dummy.o yomcst.o yomphy.o yomphy3.o acraneb_transt.F90
	$(FC) -c $(FCOPTS) acraneb_transt.F90
check_acraneb2_mod.o:  model_physics_mf_mod.o parkind1.o yomrip.o yoerdi.o load_acraneb2_mod.o check_acraneb2_mod.F90
	$(FC) -c $(FCOPTS) check_acraneb2_mod.F90
load_acraneb2_mod.o:  parkind1.o model_physics_mf_mod.o parkind1.o yomcst.o yomrip.o yoerdi.o load_yomcst_mod.o load_yomlouis_mod.o load_yomphy0_mod.o load_yomphy1_mod.o load_yomphy2_mod.o load_yomphy_mod.o load_terdi_mod.o load_model_physics_mf_type_mod.o load_trip_mod.o load_mod.o load_acraneb2_mod.F90
	$(FC) -c $(FCOPTS) load_acraneb2_mod.F90
load_model_physics_mf_type_mod.o:  model_physics_mf_mod.o load_tarphy_mod.o load_tcvmnh_mod.o load_tlouis_mod.o load_tmse_mod.o load_tnorgwd_mod.o load_tparar_mod.o load_tphy_mod.o load_tphy0_mod.o load_tphy1_mod.o load_tphy2_mod.o load_tphy3_mod.o load_tphyds_mod.o load_tsimphl_mod.o load_ttoph_mod.o load_tvdoz_mod.o load_model_physics_mf_type_mod.F90
	$(FC) -c $(FCOPTS) load_model_physics_mf_type_mod.F90
load_mod.o:  parkind1.o load_mod.F90
	$(FC) -c $(FCOPTS) load_mod.F90
load_tarphy_mod.o:  yomarphy.o load_tarphy_mod.F90
	$(FC) -c $(FCOPTS) load_tarphy_mod.F90
load_tcvmnh_mod.o:  yomcvmnh.o load_tcvmnh_mod.F90
	$(FC) -c $(FCOPTS) load_tcvmnh_mod.F90
load_terdi_mod.o:  yoerdi.o load_terdi_mod.F90
	$(FC) -c $(FCOPTS) load_terdi_mod.F90
load_tlouis_mod.o:  yomlouis.o load_tlouis_mod.F90
	$(FC) -c $(FCOPTS) load_tlouis_mod.F90
load_tmse_mod.o:  yommse.o load_tmse_mod.F90
	$(FC) -c $(FCOPTS) load_tmse_mod.F90
load_tnorgwd_mod.o:  yomnorgwd.o load_tnorgwd_mod.F90
	$(FC) -c $(FCOPTS) load_tnorgwd_mod.F90
load_tparar_mod.o:  yomparar.o load_tparar_mod.F90
	$(FC) -c $(FCOPTS) load_tparar_mod.F90
load_tphy0_mod.o:  yomphy0.o load_tphy0_mod.F90
	$(FC) -c $(FCOPTS) load_tphy0_mod.F90
load_tphy1_mod.o:  yomphy1.o load_tphy1_mod.F90
	$(FC) -c $(FCOPTS) load_tphy1_mod.F90
load_tphy2_mod.o:  yomphy2.o load_tphy2_mod.F90
	$(FC) -c $(FCOPTS) load_tphy2_mod.F90
load_tphy3_mod.o:  yomphy3.o load_tphy3_mod.F90
	$(FC) -c $(FCOPTS) load_tphy3_mod.F90
load_tphyds_mod.o:  yomphyds.o load_tphyds_mod.F90
	$(FC) -c $(FCOPTS) load_tphyds_mod.F90
load_tphy_mod.o:  yomphy.o load_tphy_mod.F90
	$(FC) -c $(FCOPTS) load_tphy_mod.F90
load_trip_mod.o:  yomrip.o load_trip_mod.F90
	$(FC) -c $(FCOPTS) load_trip_mod.F90
load_tsimphl_mod.o:  yomsimphl.o load_tsimphl_mod.F90
	$(FC) -c $(FCOPTS) load_tsimphl_mod.F90
load_ttoph_mod.o:  yomtoph.o load_ttoph_mod.F90
	$(FC) -c $(FCOPTS) load_ttoph_mod.F90
load_tvdoz_mod.o:  yomvdoz.o load_tvdoz_mod.F90
	$(FC) -c $(FCOPTS) load_tvdoz_mod.F90
load_yomcst_mod.o:  yomcst.o load_yomcst_mod.F90
	$(FC) -c $(FCOPTS) load_yomcst_mod.F90
load_yomlouis_mod.o:  yomlouis.o load_tlouis_mod.o load_yomlouis_mod.F90
	$(FC) -c $(FCOPTS) load_yomlouis_mod.F90
load_yomphy0_mod.o:  yomphy0.o load_tphy0_mod.o load_yomphy0_mod.F90
	$(FC) -c $(FCOPTS) load_yomphy0_mod.F90
load_yomphy1_mod.o:  yomphy1.o load_tphy1_mod.o load_yomphy1_mod.F90
	$(FC) -c $(FCOPTS) load_yomphy1_mod.F90
load_yomphy2_mod.o:  yomphy2.o load_tphy2_mod.o load_yomphy2_mod.F90
	$(FC) -c $(FCOPTS) load_yomphy2_mod.F90
load_yomphy_mod.o:  yomphy.o load_tphy_mod.o load_yomphy_mod.F90
	$(FC) -c $(FCOPTS) load_yomphy_mod.F90
main.o:  parkind1.o wrapper_mod.o main.F90
	$(FC) -c $(FCOPTS) main.F90
model_physics_mf_mod.o:  yomphy.o yomphy0.o yomphy1.o yomphy2.o yomphy3.o yomphyds.o yomcvmnh.o yomtoph.o yomvdoz.o yomsimphl.o yomarphy.o yomparar.o yommse.o yomlouis.o yomnorgwd.o parkind1.o model_physics_mf_mod.F90
	$(FC) -c $(FCOPTS) model_physics_mf_mod.F90
parkind1.o:  parkind1.F90
	$(FC) -c $(FCOPTS) parkind1.F90
prepare_acraneb2_mod.o:  model_physics_mf_mod.o parkind1.o yomrip.o yoerdi.o load_acraneb2_mod.o prepare_acraneb2_mod.F90
	$(FC) -c $(FCOPTS) prepare_acraneb2_mod.F90
#WARNING: no source file found containing module omp_lib
#WARNING: no source file found containing module omp_lib
wrapper_mod.o:  model_physics_mf_mod.o parkind1.o yomrip.o yoerdi.o prepare_acraneb2_mod.o check_acraneb2_mod.o model_physics_mf_mod.o parkind1.o yomrip.o yoerdi.o prepare_acraneb2_mod.o check_acraneb2_mod.o wrapper_mod.F90
	$(FC) -c $(FCOPTS) wrapper_mod.F90
yoerdi.o:  parkind1.o yoerdi.F90
	$(FC) -c $(FCOPTS) yoerdi.F90
yomarphy.o:  parkind1.o yomarphy.F90
	$(FC) -c $(FCOPTS) yomarphy.F90
yomcst.o:  parkind1.o yomcst.F90
	$(FC) -c $(FCOPTS) yomcst.F90
yomcvmnh.o:  parkind1.o yomcvmnh.F90
	$(FC) -c $(FCOPTS) yomcvmnh.F90
yomhook_dummy.o:  parkind1.o yomhook_dummy.F90
	$(FC) -c $(FCOPTS) yomhook_dummy.F90
yomlouis.o:  parkind1.o yomlouis.F90
	$(FC) -c $(FCOPTS) yomlouis.F90
yommse.o:  parkind1.o yommse.F90
	$(FC) -c $(FCOPTS) yommse.F90
yomnorgwd.o:  parkind1.o yomnorgwd.F90
	$(FC) -c $(FCOPTS) yomnorgwd.F90
yomparar.o:  parkind1.o yomparar.F90
	$(FC) -c $(FCOPTS) yomparar.F90
yomphy0.o:  parkind1.o yomphy0.F90
	$(FC) -c $(FCOPTS) yomphy0.F90
yomphy1.o:  parkind1.o yomphy1.F90
	$(FC) -c $(FCOPTS) yomphy1.F90
yomphy2.o:  parkind1.o yomphy2.F90
	$(FC) -c $(FCOPTS) yomphy2.F90
yomphy3.o:  parkind1.o yomphy3.F90
	$(FC) -c $(FCOPTS) yomphy3.F90
yomphyds.o:  parkind1.o yomphyds.F90
	$(FC) -c $(FCOPTS) yomphyds.F90
yomphy.o:  parkind1.o yomphy.F90
	$(FC) -c $(FCOPTS) yomphy.F90
yomrip.o:  parkind1.o yomrip.F90
	$(FC) -c $(FCOPTS) yomrip.F90
yomsimphl.o:  parkind1.o yomsimphl.F90
	$(FC) -c $(FCOPTS) yomsimphl.F90
yomtoph.o:  parkind1.o yomtoph.F90
	$(FC) -c $(FCOPTS) yomtoph.F90
yomvdoz.o:  parkind1.o yomvdoz.F90
	$(FC) -c $(FCOPTS) yomvdoz.F90
