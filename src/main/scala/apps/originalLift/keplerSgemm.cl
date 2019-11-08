// WARNING: This kernel depends on work-items sizes of
// localSize: 32,8,1
// globalSize: 256,128,1
// and on input dimensions of
// M = 512
// N = 256
// K = 64
#ifndef Tuple2_float_float_DEFINED
#define Tuple2_float_float_DEFINED
typedef struct __attribute__((aligned(4))){
    float _0;
    float _1;
} Tuple2_float_float;
#endif

float id(float x){
    {
        { return x; };
    }
}
float add(float x, float y){
    {
        { return x+y; };
    }
}
float mult(float l, float r){
    {
        { return l * r; };
    }
}
kernel __attribute((reqd_work_group_size(32,8,1)))
void KERNEL(const global float* restrict v__67, const global float* restrict v__68, const global float* restrict v__69, float v__70, float v__71, global float* v__106, int v_K_4, int v_M_3, int v_N_5){
    // Static local memory
    local float v__79[512];
    local float v__80[1024];
    // Typed Value memory
    float v__74;
    // Private Memory
    float v_v__75_1_839;
    float v_v__75_2_840;
    float v_v__75_3_841;
    float v_v__75_4_842;
    float v_v__75_5_843;
    float v_v__75_6_844;
    float v_v__75_7_845;
    float v_v__75_8_846;
    float v_v__75_9_847;
    float v_v__75_10_848;
    float v_v__75_11_849;
    float v_v__75_12_850;
    float v_v__75_13_851;
    float v_v__75_14_852;
    float v_v__75_15_853;
    float v_v__75_16_854;
    float v_v__75_17_855;
    float v_v__75_18_856;
    float v_v__75_19_857;
    float v_v__75_20_858;
    float v_v__75_21_859;
    float v_v__75_22_860;
    float v_v__75_23_861;
    float v_v__75_24_862;
    float v_v__75_25_863;
    float v_v__75_26_864;
    float v_v__75_27_865;
    float v_v__75_28_866;
    float v_v__75_29_867;
    float v_v__75_30_868;
    float v_v__75_31_869;
    float v_v__75_32_870;
    float v_v__86_1_871;
    float v_v__86_2_872;
    float v_v__86_3_873;
    float v_v__86_4_874;
    float v_v__86_5_875;
    float v_v__86_6_876;
    float v_v__86_7_877;
    float v_v__86_8_878;
    float v_v__87_1_879;
    float v_v__87_2_880;
    float v_v__87_3_881;
    float v_v__87_4_882;
    float v_v__92_1_883;
    float v_v__92_2_884;
    float v_v__92_3_885;
    float v_v__92_4_886;
    float v_v__92_5_887;
    float v_v__92_6_888;
    float v_v__92_7_889;
    float v_v__92_8_890;
    float v_v__92_9_891;
    float v_v__92_10_892;
    float v_v__92_11_893;
    float v_v__92_12_894;
    float v_v__92_13_895;
    float v_v__92_14_896;
    float v_v__92_15_897;
    float v_v__92_16_898;
    float v_v__92_17_899;
    float v_v__92_18_900;
    float v_v__92_19_901;
    float v_v__92_20_902;
    float v_v__92_21_903;
    float v_v__92_22_904;
    float v_v__92_23_905;
    float v_v__92_24_906;
    float v_v__92_25_907;
    float v_v__92_26_908;
    float v_v__92_27_909;
    float v_v__92_28_910;
    float v_v__92_29_911;
    float v_v__92_30_912;
    float v_v__92_31_913;
    float v_v__92_32_914;
    float v_v__101_1_915;
    float v_v__101_2_916;
    float v_v__101_3_917;
    float v_v__101_4_918;
    float v_v__101_5_919;
    float v_v__101_6_920;
    float v_v__101_7_921;
    float v_v__101_8_922;
    float v_v__101_9_923;
    float v_v__101_10_924;
    float v_v__101_11_925;
    float v_v__101_12_926;
    float v_v__101_13_927;
    float v_v__101_14_928;
    float v_v__101_15_929;
    float v_v__101_16_930;
    float v_v__101_17_931;
    float v_v__101_18_932;
    float v_v__101_19_933;
    float v_v__101_20_934;
    float v_v__101_21_935;
    float v_v__101_22_936;
    float v_v__101_23_937;
    float v_v__101_24_938;
    float v_v__101_25_939;
    float v_v__101_26_940;
    float v_v__101_27_941;
    float v_v__101_28_942;
    float v_v__101_29_943;
    float v_v__101_30_944;
    float v_v__101_31_945;
    float v_v__101_32_946;
    float v_v__104_1_947;
    float v_v__104_2_948;
    float v_v__104_3_949;
    float v_v__104_4_950;
    float v_v__104_5_951;
    float v_v__104_6_952;
    float v_v__104_7_953;
    float v_v__104_8_954;
    float v_v__104_9_955;
    float v_v__104_10_956;
    float v_v__104_11_957;
    float v_v__104_12_958;
    float v_v__104_13_959;
    float v_v__104_14_960;
    float v_v__104_15_961;
    float v_v__104_16_962;
    float v_v__104_17_963;
    float v_v__104_18_964;
    float v_v__104_19_965;
    float v_v__104_20_966;
    float v_v__104_21_967;
    float v_v__104_22_968;
    float v_v__104_23_969;
    float v_v__104_24_970;
    float v_v__104_25_971;
    float v_v__104_26_972;
    float v_v__104_27_973;
    float v_v__104_28_974;
    float v_v__104_29_975;
    float v_v__104_30_976;
    float v_v__104_31_977;
    float v_v__104_32_978;
    // iteration count is exactly 1 or less, no loop emitted
    if ((get_group_id(1) < 8)){
        int v_wg_id_40 = get_group_id(1);
        // iteration count is exactly 1 or less, no loop emitted
        if ((get_group_id(0) < 2)){
            int v_wg_id_41 = get_group_id(0);
            float v_tmp_421 = 0.0f;
            v__74 = v_tmp_421;
            // unroll
            // unroll
            // map_seq
            // unroll
            // map_seq
            // unroll
            v_v__75_1_839 = id(v__74);
            v_v__75_2_840 = id(v__74);
            v_v__75_3_841 = id(v__74);
            v_v__75_4_842 = id(v__74);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__75_5_843 = id(v__74);
            v_v__75_6_844 = id(v__74);
            v_v__75_7_845 = id(v__74);
            v_v__75_8_846 = id(v__74);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__75_9_847 = id(v__74);
            v_v__75_10_848 = id(v__74);
            v_v__75_11_849 = id(v__74);
            v_v__75_12_850 = id(v__74);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__75_13_851 = id(v__74);
            v_v__75_14_852 = id(v__74);
            v_v__75_15_853 = id(v__74);
            v_v__75_16_854 = id(v__74);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__75_17_855 = id(v__74);
            v_v__75_18_856 = id(v__74);
            v_v__75_19_857 = id(v__74);
            v_v__75_20_858 = id(v__74);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__75_21_859 = id(v__74);
            v_v__75_22_860 = id(v__74);
            v_v__75_23_861 = id(v__74);
            v_v__75_24_862 = id(v__74);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__75_25_863 = id(v__74);
            v_v__75_26_864 = id(v__74);
            v_v__75_27_865 = id(v__74);
            v_v__75_28_866 = id(v__74);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__75_29_867 = id(v__74);
            v_v__75_30_868 = id(v__74);
            v_v__75_31_869 = id(v__74);
            v_v__75_32_870 = id(v__74);
            // end unroll
            // end map_seq
            // end unroll
            // end map_seq
            // end unroll
            // end unroll
            // reduce_seq
            for (int v_i_46 = 0; (v_i_46 < ((v_K_4)/(8))); v_i_46 = (1 + v_i_46)){
                // iteration count is exactly 1, no loop emitted
                {
                    int v_l_id_47 = get_local_id(1);
                    for (int v_l_id_50 = get_local_id(0); (v_l_id_50 < 64); v_l_id_50 = (32 + v_l_id_50)){
                        v__79[(v_l_id_50 + (64 * v_l_id_47))] = id(v__67[(v_l_id_50 + (8 * v_M_3 * v_i_46) + (8 * v_M_3 * (v_wg_id_40 / ((v_M_3)/(64)))) + (v_M_3 * v_l_id_47) + (64 * (v_wg_id_40 % ((v_M_3)/(64)))))]);
                    }
                    barrier(CLK_LOCAL_MEM_FENCE);
                    for (int v_l_id_51 = get_local_id(0); (v_l_id_51 < 128); v_l_id_51 = (32 + v_l_id_51)){
                        v__80[(v_l_id_51 + (128 * v_l_id_47))] = id(v__68[(v_l_id_51 + (8 * v_N_5 * v_i_46) + (8 * v_N_5 * (v_wg_id_41 / ((v_N_5)/(128)))) + (v_N_5 * v_l_id_47) + (128 * (v_wg_id_41 % ((v_N_5)/(128)))))]);
                    }
                    barrier(CLK_LOCAL_MEM_FENCE);
                }
                barrier(CLK_LOCAL_MEM_FENCE);
                // unroll
                // unroll
                // reduce_seq
                for (int v_i_54 = 0; (v_i_54 < 8); v_i_54 = (1 + v_i_54)){
                    // map_seq
                    // unroll
                    v_v__86_1_871 = id(v__79[((64 * v_i_54) + (8 * get_local_id(1)))]);
                    v_v__86_2_872 = id(v__79[(1 + (8 * get_local_id(1)) + (64 * v_i_54))]);
                    v_v__86_3_873 = id(v__79[(2 + (8 * get_local_id(1)) + (64 * v_i_54))]);
                    v_v__86_4_874 = id(v__79[(3 + (8 * get_local_id(1)) + (64 * v_i_54))]);
                    v_v__86_5_875 = id(v__79[(4 + (8 * get_local_id(1)) + (64 * v_i_54))]);
                    v_v__86_6_876 = id(v__79[(5 + (8 * get_local_id(1)) + (64 * v_i_54))]);
                    v_v__86_7_877 = id(v__79[(6 + (8 * get_local_id(1)) + (64 * v_i_54))]);
                    v_v__86_8_878 = id(v__79[(7 + (8 * get_local_id(1)) + (64 * v_i_54))]);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__87_1_879 = id(v__80[((128 * v_i_54) + get_local_id(0))]);
                    v_v__87_2_880 = id(v__80[(32 + (128 * v_i_54) + get_local_id(0))]);
                    v_v__87_3_881 = id(v__80[(64 + (128 * v_i_54) + get_local_id(0))]);
                    v_v__87_4_882 = id(v__80[(96 + (128 * v_i_54) + get_local_id(0))]);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    // map_seq
                    // unroll
                    v_v__92_1_883 = mult(v_v__86_1_871, v_v__87_1_879);
                    v_v__75_1_839 = add(v_v__75_1_839, v_v__92_1_883);
                    v_v__92_2_884 = mult(v_v__86_1_871, v_v__87_2_880);
                    v_v__75_2_840 = add(v_v__75_2_840, v_v__92_2_884);
                    v_v__92_3_885 = mult(v_v__86_1_871, v_v__87_3_881);
                    v_v__75_3_841 = add(v_v__75_3_841, v_v__92_3_885);
                    v_v__92_4_886 = mult(v_v__86_1_871, v_v__87_4_882);
                    v_v__75_4_842 = add(v_v__75_4_842, v_v__92_4_886);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__92_5_887 = mult(v_v__86_2_872, v_v__87_1_879);
                    v_v__75_5_843 = add(v_v__75_5_843, v_v__92_5_887);
                    v_v__92_6_888 = mult(v_v__86_2_872, v_v__87_2_880);
                    v_v__75_6_844 = add(v_v__75_6_844, v_v__92_6_888);
                    v_v__92_7_889 = mult(v_v__86_2_872, v_v__87_3_881);
                    v_v__75_7_845 = add(v_v__75_7_845, v_v__92_7_889);
                    v_v__92_8_890 = mult(v_v__86_2_872, v_v__87_4_882);
                    v_v__75_8_846 = add(v_v__75_8_846, v_v__92_8_890);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__92_9_891 = mult(v_v__86_3_873, v_v__87_1_879);
                    v_v__75_9_847 = add(v_v__75_9_847, v_v__92_9_891);
                    v_v__92_10_892 = mult(v_v__86_3_873, v_v__87_2_880);
                    v_v__75_10_848 = add(v_v__75_10_848, v_v__92_10_892);
                    v_v__92_11_893 = mult(v_v__86_3_873, v_v__87_3_881);
                    v_v__75_11_849 = add(v_v__75_11_849, v_v__92_11_893);
                    v_v__92_12_894 = mult(v_v__86_3_873, v_v__87_4_882);
                    v_v__75_12_850 = add(v_v__75_12_850, v_v__92_12_894);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__92_13_895 = mult(v_v__86_4_874, v_v__87_1_879);
                    v_v__75_13_851 = add(v_v__75_13_851, v_v__92_13_895);
                    v_v__92_14_896 = mult(v_v__86_4_874, v_v__87_2_880);
                    v_v__75_14_852 = add(v_v__75_14_852, v_v__92_14_896);
                    v_v__92_15_897 = mult(v_v__86_4_874, v_v__87_3_881);
                    v_v__75_15_853 = add(v_v__75_15_853, v_v__92_15_897);
                    v_v__92_16_898 = mult(v_v__86_4_874, v_v__87_4_882);
                    v_v__75_16_854 = add(v_v__75_16_854, v_v__92_16_898);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__92_17_899 = mult(v_v__86_5_875, v_v__87_1_879);
                    v_v__75_17_855 = add(v_v__75_17_855, v_v__92_17_899);
                    v_v__92_18_900 = mult(v_v__86_5_875, v_v__87_2_880);
                    v_v__75_18_856 = add(v_v__75_18_856, v_v__92_18_900);
                    v_v__92_19_901 = mult(v_v__86_5_875, v_v__87_3_881);
                    v_v__75_19_857 = add(v_v__75_19_857, v_v__92_19_901);
                    v_v__92_20_902 = mult(v_v__86_5_875, v_v__87_4_882);
                    v_v__75_20_858 = add(v_v__75_20_858, v_v__92_20_902);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__92_21_903 = mult(v_v__86_6_876, v_v__87_1_879);
                    v_v__75_21_859 = add(v_v__75_21_859, v_v__92_21_903);
                    v_v__92_22_904 = mult(v_v__86_6_876, v_v__87_2_880);
                    v_v__75_22_860 = add(v_v__75_22_860, v_v__92_22_904);
                    v_v__92_23_905 = mult(v_v__86_6_876, v_v__87_3_881);
                    v_v__75_23_861 = add(v_v__75_23_861, v_v__92_23_905);
                    v_v__92_24_906 = mult(v_v__86_6_876, v_v__87_4_882);
                    v_v__75_24_862 = add(v_v__75_24_862, v_v__92_24_906);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__92_25_907 = mult(v_v__86_7_877, v_v__87_1_879);
                    v_v__75_25_863 = add(v_v__75_25_863, v_v__92_25_907);
                    v_v__92_26_908 = mult(v_v__86_7_877, v_v__87_2_880);
                    v_v__75_26_864 = add(v_v__75_26_864, v_v__92_26_908);
                    v_v__92_27_909 = mult(v_v__86_7_877, v_v__87_3_881);
                    v_v__75_27_865 = add(v_v__75_27_865, v_v__92_27_909);
                    v_v__92_28_910 = mult(v_v__86_7_877, v_v__87_4_882);
                    v_v__75_28_866 = add(v_v__75_28_866, v_v__92_28_910);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__92_29_911 = mult(v_v__86_8_878, v_v__87_1_879);
                    v_v__75_29_867 = add(v_v__75_29_867, v_v__92_29_911);
                    v_v__92_30_912 = mult(v_v__86_8_878, v_v__87_2_880);
                    v_v__75_30_868 = add(v_v__75_30_868, v_v__92_30_912);
                    v_v__92_31_913 = mult(v_v__86_8_878, v_v__87_3_881);
                    v_v__75_31_869 = add(v_v__75_31_869, v_v__92_31_913);
                    v_v__92_32_914 = mult(v_v__86_8_878, v_v__87_4_882);
                    v_v__75_32_870 = add(v_v__75_32_870, v_v__92_32_914);
                    // end unroll
                    // end map_seq
                    // end unroll
                    // end map_seq
                }
                // end reduce_seq
                // map_seq
                // unroll
                // end unroll
                // end map_seq
                // end unroll
                // end unroll
                barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
            }
            // end reduce_seq
            // map_seq
            // unroll
            // unroll
            // unroll
            // map_seq
            // unroll
            // map_seq
            // unroll
            v_v__101_1_915 = mult(v_v__75_1_839, v__70);
            v_v__104_1_947 = mult(v__69[((8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[((8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_1_915, v_v__104_1_947);
            v_v__101_2_916 = mult(v_v__75_2_840, v__70);
            v_v__104_2_948 = mult(v__69[(32 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_2_916, v_v__104_2_948);
            v_v__101_3_917 = mult(v_v__75_3_841, v__70);
            v_v__104_3_949 = mult(v__69[(64 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_3_917, v_v__104_3_949);
            v_v__101_4_918 = mult(v_v__75_4_842, v__70);
            v_v__104_4_950 = mult(v__69[(96 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_4_918, v_v__104_4_950);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__101_5_919 = mult(v_v__75_5_843, v__70);
            v_v__104_5_951 = mult(v__69[(v_N_5 + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(v_N_5 + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_5_919, v_v__104_5_951);
            v_v__101_6_920 = mult(v_v__75_6_844, v__70);
            v_v__104_6_952 = mult(v__69[(32 + v_N_5 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + v_N_5 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_6_920, v_v__104_6_952);
            v_v__101_7_921 = mult(v_v__75_7_845, v__70);
            v_v__104_7_953 = mult(v__69[(64 + v_N_5 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + v_N_5 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_7_921, v_v__104_7_953);
            v_v__101_8_922 = mult(v_v__75_8_846, v__70);
            v_v__104_8_954 = mult(v__69[(96 + v_N_5 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + v_N_5 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_8_922, v_v__104_8_954);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__101_9_923 = mult(v_v__75_9_847, v__70);
            v_v__104_9_955 = mult(v__69[((2 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[((2 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_9_923, v_v__104_9_955);
            v_v__101_10_924 = mult(v_v__75_10_848, v__70);
            v_v__104_10_956 = mult(v__69[(32 + (2 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + (2 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_10_924, v_v__104_10_956);
            v_v__101_11_925 = mult(v_v__75_11_849, v__70);
            v_v__104_11_957 = mult(v__69[(64 + (2 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + (2 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_11_925, v_v__104_11_957);
            v_v__101_12_926 = mult(v_v__75_12_850, v__70);
            v_v__104_12_958 = mult(v__69[(96 + (2 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + (2 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_12_926, v_v__104_12_958);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__101_13_927 = mult(v_v__75_13_851, v__70);
            v_v__104_13_959 = mult(v__69[((3 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[((3 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_13_927, v_v__104_13_959);
            v_v__101_14_928 = mult(v_v__75_14_852, v__70);
            v_v__104_14_960 = mult(v__69[(32 + (3 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + (3 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_14_928, v_v__104_14_960);
            v_v__101_15_929 = mult(v_v__75_15_853, v__70);
            v_v__104_15_961 = mult(v__69[(64 + (3 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + (3 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_15_929, v_v__104_15_961);
            v_v__101_16_930 = mult(v_v__75_16_854, v__70);
            v_v__104_16_962 = mult(v__69[(96 + (3 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + (3 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_16_930, v_v__104_16_962);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__101_17_931 = mult(v_v__75_17_855, v__70);
            v_v__104_17_963 = mult(v__69[((4 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[((4 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_17_931, v_v__104_17_963);
            v_v__101_18_932 = mult(v_v__75_18_856, v__70);
            v_v__104_18_964 = mult(v__69[(32 + (4 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + (4 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_18_932, v_v__104_18_964);
            v_v__101_19_933 = mult(v_v__75_19_857, v__70);
            v_v__104_19_965 = mult(v__69[(64 + (4 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + (4 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_19_933, v_v__104_19_965);
            v_v__101_20_934 = mult(v_v__75_20_858, v__70);
            v_v__104_20_966 = mult(v__69[(96 + (4 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + (4 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_20_934, v_v__104_20_966);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__101_21_935 = mult(v_v__75_21_859, v__70);
            v_v__104_21_967 = mult(v__69[((5 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[((5 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_21_935, v_v__104_21_967);
            v_v__101_22_936 = mult(v_v__75_22_860, v__70);
            v_v__104_22_968 = mult(v__69[(32 + (5 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + (5 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_22_936, v_v__104_22_968);
            v_v__101_23_937 = mult(v_v__75_23_861, v__70);
            v_v__104_23_969 = mult(v__69[(64 + (5 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + (5 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_23_937, v_v__104_23_969);
            v_v__101_24_938 = mult(v_v__75_24_862, v__70);
            v_v__104_24_970 = mult(v__69[(96 + (5 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + (5 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_24_938, v_v__104_24_970);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__101_25_939 = mult(v_v__75_25_863, v__70);
            v_v__104_25_971 = mult(v__69[((6 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[((6 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_25_939, v_v__104_25_971);
            v_v__101_26_940 = mult(v_v__75_26_864, v__70);
            v_v__104_26_972 = mult(v__69[(32 + (6 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + (6 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_26_940, v_v__104_26_972);
            v_v__101_27_941 = mult(v_v__75_27_865, v__70);
            v_v__104_27_973 = mult(v__69[(64 + (6 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + (6 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_27_941, v_v__104_27_973);
            v_v__101_28_942 = mult(v_v__75_28_866, v__70);
            v_v__104_28_974 = mult(v__69[(96 + (6 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + (6 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_28_942, v_v__104_28_974);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__101_29_943 = mult(v_v__75_29_867, v__70);
            v_v__104_29_975 = mult(v__69[((7 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[((7 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_29_943, v_v__104_29_975);
            v_v__101_30_944 = mult(v_v__75_30_868, v__70);
            v_v__104_30_976 = mult(v__69[(32 + (7 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + (7 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_30_944, v_v__104_30_976);
            v_v__101_31_945 = mult(v_v__75_31_869, v__70);
            v_v__104_31_977 = mult(v__69[(64 + (7 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + (7 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_31_945, v_v__104_31_977);
            v_v__101_32_946 = mult(v_v__75_32_870, v__70);
            v_v__104_32_978 = mult(v__69[(96 + (7 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + (7 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_32_946, v_v__104_32_978);
            // end unroll
            // end map_seq
            // end unroll
            // end map_seq
            // end unroll
            // end unroll
            // end unroll
            // end map_seq
        }
    }
}
