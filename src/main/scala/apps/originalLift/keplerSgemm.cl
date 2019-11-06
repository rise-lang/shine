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
    float v_v__75_1_835;
    float v_v__75_2_836;
    float v_v__75_3_837;
    float v_v__75_4_838;
    float v_v__75_5_839;
    float v_v__75_6_840;
    float v_v__75_7_841;
    float v_v__75_8_842;
    float v_v__75_9_843;
    float v_v__75_10_844;
    float v_v__75_11_845;
    float v_v__75_12_846;
    float v_v__75_13_847;
    float v_v__75_14_848;
    float v_v__75_15_849;
    float v_v__75_16_850;
    float v_v__75_17_851;
    float v_v__75_18_852;
    float v_v__75_19_853;
    float v_v__75_20_854;
    float v_v__75_21_855;
    float v_v__75_22_856;
    float v_v__75_23_857;
    float v_v__75_24_858;
    float v_v__75_25_859;
    float v_v__75_26_860;
    float v_v__75_27_861;
    float v_v__75_28_862;
    float v_v__75_29_863;
    float v_v__75_30_864;
    float v_v__75_31_865;
    float v_v__75_32_866;
    float v_v__86_1_867;
    float v_v__86_2_868;
    float v_v__86_3_869;
    float v_v__86_4_870;
    float v_v__86_5_871;
    float v_v__86_6_872;
    float v_v__86_7_873;
    float v_v__86_8_874;
    float v_v__87_1_875;
    float v_v__87_2_876;
    float v_v__87_3_877;
    float v_v__87_4_878;
    float v_v__92_1_879;
    float v_v__92_2_880;
    float v_v__92_3_881;
    float v_v__92_4_882;
    float v_v__92_5_883;
    float v_v__92_6_884;
    float v_v__92_7_885;
    float v_v__92_8_886;
    float v_v__92_9_887;
    float v_v__92_10_888;
    float v_v__92_11_889;
    float v_v__92_12_890;
    float v_v__92_13_891;
    float v_v__92_14_892;
    float v_v__92_15_893;
    float v_v__92_16_894;
    float v_v__92_17_895;
    float v_v__92_18_896;
    float v_v__92_19_897;
    float v_v__92_20_898;
    float v_v__92_21_899;
    float v_v__92_22_900;
    float v_v__92_23_901;
    float v_v__92_24_902;
    float v_v__92_25_903;
    float v_v__92_26_904;
    float v_v__92_27_905;
    float v_v__92_28_906;
    float v_v__92_29_907;
    float v_v__92_30_908;
    float v_v__92_31_909;
    float v_v__92_32_910;
    float v_v__101_1_911;
    float v_v__101_2_912;
    float v_v__101_3_913;
    float v_v__101_4_914;
    float v_v__101_5_915;
    float v_v__101_6_916;
    float v_v__101_7_917;
    float v_v__101_8_918;
    float v_v__101_9_919;
    float v_v__101_10_920;
    float v_v__101_11_921;
    float v_v__101_12_922;
    float v_v__101_13_923;
    float v_v__101_14_924;
    float v_v__101_15_925;
    float v_v__101_16_926;
    float v_v__101_17_927;
    float v_v__101_18_928;
    float v_v__101_19_929;
    float v_v__101_20_930;
    float v_v__101_21_931;
    float v_v__101_22_932;
    float v_v__101_23_933;
    float v_v__101_24_934;
    float v_v__101_25_935;
    float v_v__101_26_936;
    float v_v__101_27_937;
    float v_v__101_28_938;
    float v_v__101_29_939;
    float v_v__101_30_940;
    float v_v__101_31_941;
    float v_v__101_32_942;
    float v_v__104_1_943;
    float v_v__104_2_944;
    float v_v__104_3_945;
    float v_v__104_4_946;
    float v_v__104_5_947;
    float v_v__104_6_948;
    float v_v__104_7_949;
    float v_v__104_8_950;
    float v_v__104_9_951;
    float v_v__104_10_952;
    float v_v__104_11_953;
    float v_v__104_12_954;
    float v_v__104_13_955;
    float v_v__104_14_956;
    float v_v__104_15_957;
    float v_v__104_16_958;
    float v_v__104_17_959;
    float v_v__104_18_960;
    float v_v__104_19_961;
    float v_v__104_20_962;
    float v_v__104_21_963;
    float v_v__104_22_964;
    float v_v__104_23_965;
    float v_v__104_24_966;
    float v_v__104_25_967;
    float v_v__104_26_968;
    float v_v__104_27_969;
    float v_v__104_28_970;
    float v_v__104_29_971;
    float v_v__104_30_972;
    float v_v__104_31_973;
    float v_v__104_32_974;
    // iteration count is exactly 1 or less, no loop emitted
    if ((get_group_id(1) < 1)){
        int v_wg_id_40 = get_group_id(1);
        // iteration count is exactly 1 or less, no loop emitted
        if ((get_group_id(0) < 1)){
            int v_wg_id_41 = get_group_id(0);
            float v_tmp_421 = 0.0f;
            v__74 = v_tmp_421;
            // unroll
            // unroll
            // map_seq
            // unroll
            // map_seq
            // unroll
            v_v__75_1_835 = id(v__74);
            v_v__75_2_836 = id(v__74);
            v_v__75_3_837 = id(v__74);
            v_v__75_4_838 = id(v__74);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__75_5_839 = id(v__74);
            v_v__75_6_840 = id(v__74);
            v_v__75_7_841 = id(v__74);
            v_v__75_8_842 = id(v__74);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__75_9_843 = id(v__74);
            v_v__75_10_844 = id(v__74);
            v_v__75_11_845 = id(v__74);
            v_v__75_12_846 = id(v__74);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__75_13_847 = id(v__74);
            v_v__75_14_848 = id(v__74);
            v_v__75_15_849 = id(v__74);
            v_v__75_16_850 = id(v__74);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__75_17_851 = id(v__74);
            v_v__75_18_852 = id(v__74);
            v_v__75_19_853 = id(v__74);
            v_v__75_20_854 = id(v__74);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__75_21_855 = id(v__74);
            v_v__75_22_856 = id(v__74);
            v_v__75_23_857 = id(v__74);
            v_v__75_24_858 = id(v__74);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__75_25_859 = id(v__74);
            v_v__75_26_860 = id(v__74);
            v_v__75_27_861 = id(v__74);
            v_v__75_28_862 = id(v__74);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__75_29_863 = id(v__74);
            v_v__75_30_864 = id(v__74);
            v_v__75_31_865 = id(v__74);
            v_v__75_32_866 = id(v__74);
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
                        v__79[(v_l_id_50 + (64 * v_l_id_47))] = id(v__67[(v_l_id_50 + (v_M_3 * v_K_4 * v_wg_id_40) + (8 * v_M_3 * v_i_46) + (v_M_3 * v_l_id_47))]);
                    }
                    barrier(CLK_LOCAL_MEM_FENCE);
                    for (int v_l_id_51 = get_local_id(0); (v_l_id_51 < 128); v_l_id_51 = (32 + v_l_id_51)){
                        v__80[(v_l_id_51 + (128 * v_l_id_47))] = id(v__68[(v_l_id_51 + (v_K_4 * v_N_5 * v_wg_id_41) + (8 * v_N_5 * v_i_46) + (v_N_5 * v_l_id_47))]);
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
                    v_v__86_1_867 = id(v__79[((64 * v_i_54) + (8 * get_local_id(1)))]);
                    v_v__86_2_868 = id(v__79[(1 + (8 * get_local_id(1)) + (64 * v_i_54))]);
                    v_v__86_3_869 = id(v__79[(2 + (8 * get_local_id(1)) + (64 * v_i_54))]);
                    v_v__86_4_870 = id(v__79[(3 + (8 * get_local_id(1)) + (64 * v_i_54))]);
                    v_v__86_5_871 = id(v__79[(4 + (8 * get_local_id(1)) + (64 * v_i_54))]);
                    v_v__86_6_872 = id(v__79[(5 + (8 * get_local_id(1)) + (64 * v_i_54))]);
                    v_v__86_7_873 = id(v__79[(6 + (8 * get_local_id(1)) + (64 * v_i_54))]);
                    v_v__86_8_874 = id(v__79[(7 + (8 * get_local_id(1)) + (64 * v_i_54))]);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__87_1_875 = id(v__80[((128 * v_i_54) + get_local_id(0))]);
                    v_v__87_2_876 = id(v__80[(32 + (128 * v_i_54) + get_local_id(0))]);
                    v_v__87_3_877 = id(v__80[(64 + (128 * v_i_54) + get_local_id(0))]);
                    v_v__87_4_878 = id(v__80[(96 + (128 * v_i_54) + get_local_id(0))]);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    // map_seq
                    // unroll
                    v_v__92_1_879 = mult(v_v__86_1_867, v_v__87_1_875);
                    v_v__75_1_835 = add(v_v__75_1_835, v_v__92_1_879);
                    v_v__92_2_880 = mult(v_v__86_1_867, v_v__87_2_876);
                    v_v__75_2_836 = add(v_v__75_2_836, v_v__92_2_880);
                    v_v__92_3_881 = mult(v_v__86_1_867, v_v__87_3_877);
                    v_v__75_3_837 = add(v_v__75_3_837, v_v__92_3_881);
                    v_v__92_4_882 = mult(v_v__86_1_867, v_v__87_4_878);
                    v_v__75_4_838 = add(v_v__75_4_838, v_v__92_4_882);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__92_5_883 = mult(v_v__86_2_868, v_v__87_1_875);
                    v_v__75_5_839 = add(v_v__75_5_839, v_v__92_5_883);
                    v_v__92_6_884 = mult(v_v__86_2_868, v_v__87_2_876);
                    v_v__75_6_840 = add(v_v__75_6_840, v_v__92_6_884);
                    v_v__92_7_885 = mult(v_v__86_2_868, v_v__87_3_877);
                    v_v__75_7_841 = add(v_v__75_7_841, v_v__92_7_885);
                    v_v__92_8_886 = mult(v_v__86_2_868, v_v__87_4_878);
                    v_v__75_8_842 = add(v_v__75_8_842, v_v__92_8_886);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__92_9_887 = mult(v_v__86_3_869, v_v__87_1_875);
                    v_v__75_9_843 = add(v_v__75_9_843, v_v__92_9_887);
                    v_v__92_10_888 = mult(v_v__86_3_869, v_v__87_2_876);
                    v_v__75_10_844 = add(v_v__75_10_844, v_v__92_10_888);
                    v_v__92_11_889 = mult(v_v__86_3_869, v_v__87_3_877);
                    v_v__75_11_845 = add(v_v__75_11_845, v_v__92_11_889);
                    v_v__92_12_890 = mult(v_v__86_3_869, v_v__87_4_878);
                    v_v__75_12_846 = add(v_v__75_12_846, v_v__92_12_890);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__92_13_891 = mult(v_v__86_4_870, v_v__87_1_875);
                    v_v__75_13_847 = add(v_v__75_13_847, v_v__92_13_891);
                    v_v__92_14_892 = mult(v_v__86_4_870, v_v__87_2_876);
                    v_v__75_14_848 = add(v_v__75_14_848, v_v__92_14_892);
                    v_v__92_15_893 = mult(v_v__86_4_870, v_v__87_3_877);
                    v_v__75_15_849 = add(v_v__75_15_849, v_v__92_15_893);
                    v_v__92_16_894 = mult(v_v__86_4_870, v_v__87_4_878);
                    v_v__75_16_850 = add(v_v__75_16_850, v_v__92_16_894);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__92_17_895 = mult(v_v__86_5_871, v_v__87_1_875);
                    v_v__75_17_851 = add(v_v__75_17_851, v_v__92_17_895);
                    v_v__92_18_896 = mult(v_v__86_5_871, v_v__87_2_876);
                    v_v__75_18_852 = add(v_v__75_18_852, v_v__92_18_896);
                    v_v__92_19_897 = mult(v_v__86_5_871, v_v__87_3_877);
                    v_v__75_19_853 = add(v_v__75_19_853, v_v__92_19_897);
                    v_v__92_20_898 = mult(v_v__86_5_871, v_v__87_4_878);
                    v_v__75_20_854 = add(v_v__75_20_854, v_v__92_20_898);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__92_21_899 = mult(v_v__86_6_872, v_v__87_1_875);
                    v_v__75_21_855 = add(v_v__75_21_855, v_v__92_21_899);
                    v_v__92_22_900 = mult(v_v__86_6_872, v_v__87_2_876);
                    v_v__75_22_856 = add(v_v__75_22_856, v_v__92_22_900);
                    v_v__92_23_901 = mult(v_v__86_6_872, v_v__87_3_877);
                    v_v__75_23_857 = add(v_v__75_23_857, v_v__92_23_901);
                    v_v__92_24_902 = mult(v_v__86_6_872, v_v__87_4_878);
                    v_v__75_24_858 = add(v_v__75_24_858, v_v__92_24_902);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__92_25_903 = mult(v_v__86_7_873, v_v__87_1_875);
                    v_v__75_25_859 = add(v_v__75_25_859, v_v__92_25_903);
                    v_v__92_26_904 = mult(v_v__86_7_873, v_v__87_2_876);
                    v_v__75_26_860 = add(v_v__75_26_860, v_v__92_26_904);
                    v_v__92_27_905 = mult(v_v__86_7_873, v_v__87_3_877);
                    v_v__75_27_861 = add(v_v__75_27_861, v_v__92_27_905);
                    v_v__92_28_906 = mult(v_v__86_7_873, v_v__87_4_878);
                    v_v__75_28_862 = add(v_v__75_28_862, v_v__92_28_906);
                    // end unroll
                    // end map_seq
                    // map_seq
                    // unroll
                    v_v__92_29_907 = mult(v_v__86_8_874, v_v__87_1_875);
                    v_v__75_29_863 = add(v_v__75_29_863, v_v__92_29_907);
                    v_v__92_30_908 = mult(v_v__86_8_874, v_v__87_2_876);
                    v_v__75_30_864 = add(v_v__75_30_864, v_v__92_30_908);
                    v_v__92_31_909 = mult(v_v__86_8_874, v_v__87_3_877);
                    v_v__75_31_865 = add(v_v__75_31_865, v_v__92_31_909);
                    v_v__92_32_910 = mult(v_v__86_8_874, v_v__87_4_878);
                    v_v__75_32_866 = add(v_v__75_32_866, v_v__92_32_910);
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
            v_v__101_1_911 = mult(v_v__75_1_835, v__70);
            v_v__104_1_943 = mult(v__69[((8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[((8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_1_911, v_v__104_1_943);
            v_v__101_2_912 = mult(v_v__75_2_836, v__70);
            v_v__104_2_944 = mult(v__69[(32 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_2_912, v_v__104_2_944);
            v_v__101_3_913 = mult(v_v__75_3_837, v__70);
            v_v__104_3_945 = mult(v__69[(64 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_3_913, v_v__104_3_945);
            v_v__101_4_914 = mult(v_v__75_4_838, v__70);
            v_v__104_4_946 = mult(v__69[(96 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_4_914, v_v__104_4_946);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__101_5_915 = mult(v_v__75_5_839, v__70);
            v_v__104_5_947 = mult(v__69[(v_N_5 + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(v_N_5 + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_5_915, v_v__104_5_947);
            v_v__101_6_916 = mult(v_v__75_6_840, v__70);
            v_v__104_6_948 = mult(v__69[(32 + v_N_5 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + v_N_5 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_6_916, v_v__104_6_948);
            v_v__101_7_917 = mult(v_v__75_7_841, v__70);
            v_v__104_7_949 = mult(v__69[(64 + v_N_5 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + v_N_5 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_7_917, v_v__104_7_949);
            v_v__101_8_918 = mult(v_v__75_8_842, v__70);
            v_v__104_8_950 = mult(v__69[(96 + v_N_5 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + v_N_5 + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_8_918, v_v__104_8_950);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__101_9_919 = mult(v_v__75_9_843, v__70);
            v_v__104_9_951 = mult(v__69[((2 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[((2 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_9_919, v_v__104_9_951);
            v_v__101_10_920 = mult(v_v__75_10_844, v__70);
            v_v__104_10_952 = mult(v__69[(32 + (2 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + (2 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_10_920, v_v__104_10_952);
            v_v__101_11_921 = mult(v_v__75_11_845, v__70);
            v_v__104_11_953 = mult(v__69[(64 + (2 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + (2 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_11_921, v_v__104_11_953);
            v_v__101_12_922 = mult(v_v__75_12_846, v__70);
            v_v__104_12_954 = mult(v__69[(96 + (2 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + (2 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_12_922, v_v__104_12_954);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__101_13_923 = mult(v_v__75_13_847, v__70);
            v_v__104_13_955 = mult(v__69[((3 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[((3 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_13_923, v_v__104_13_955);
            v_v__101_14_924 = mult(v_v__75_14_848, v__70);
            v_v__104_14_956 = mult(v__69[(32 + (3 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + (3 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_14_924, v_v__104_14_956);
            v_v__101_15_925 = mult(v_v__75_15_849, v__70);
            v_v__104_15_957 = mult(v__69[(64 + (3 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + (3 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_15_925, v_v__104_15_957);
            v_v__101_16_926 = mult(v_v__75_16_850, v__70);
            v_v__104_16_958 = mult(v__69[(96 + (3 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + (3 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_16_926, v_v__104_16_958);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__101_17_927 = mult(v_v__75_17_851, v__70);
            v_v__104_17_959 = mult(v__69[((4 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[((4 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_17_927, v_v__104_17_959);
            v_v__101_18_928 = mult(v_v__75_18_852, v__70);
            v_v__104_18_960 = mult(v__69[(32 + (4 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + (4 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_18_928, v_v__104_18_960);
            v_v__101_19_929 = mult(v_v__75_19_853, v__70);
            v_v__104_19_961 = mult(v__69[(64 + (4 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + (4 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_19_929, v_v__104_19_961);
            v_v__101_20_930 = mult(v_v__75_20_854, v__70);
            v_v__104_20_962 = mult(v__69[(96 + (4 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + (4 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_20_930, v_v__104_20_962);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__101_21_931 = mult(v_v__75_21_855, v__70);
            v_v__104_21_963 = mult(v__69[((5 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[((5 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_21_931, v_v__104_21_963);
            v_v__101_22_932 = mult(v_v__75_22_856, v__70);
            v_v__104_22_964 = mult(v__69[(32 + (5 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + (5 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_22_932, v_v__104_22_964);
            v_v__101_23_933 = mult(v_v__75_23_857, v__70);
            v_v__104_23_965 = mult(v__69[(64 + (5 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + (5 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_23_933, v_v__104_23_965);
            v_v__101_24_934 = mult(v_v__75_24_858, v__70);
            v_v__104_24_966 = mult(v__69[(96 + (5 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + (5 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_24_934, v_v__104_24_966);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__101_25_935 = mult(v_v__75_25_859, v__70);
            v_v__104_25_967 = mult(v__69[((6 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[((6 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_25_935, v_v__104_25_967);
            v_v__101_26_936 = mult(v_v__75_26_860, v__70);
            v_v__104_26_968 = mult(v__69[(32 + (6 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + (6 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_26_936, v_v__104_26_968);
            v_v__101_27_937 = mult(v_v__75_27_861, v__70);
            v_v__104_27_969 = mult(v__69[(64 + (6 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + (6 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_27_937, v_v__104_27_969);
            v_v__101_28_938 = mult(v_v__75_28_862, v__70);
            v_v__104_28_970 = mult(v__69[(96 + (6 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + (6 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_28_938, v_v__104_28_970);
            // end unroll
            // end map_seq
            // map_seq
            // unroll
            v_v__101_29_939 = mult(v_v__75_29_863, v__70);
            v_v__104_29_971 = mult(v__69[((7 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[((7 * v_N_5) + (8 * v_N_5 * get_local_id(1)) + (64 * v_N_5 * v_wg_id_40) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_29_939, v_v__104_29_971);
            v_v__101_30_940 = mult(v_v__75_30_864, v__70);
            v_v__104_30_972 = mult(v__69[(32 + (7 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(32 + (7 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_30_940, v_v__104_30_972);
            v_v__101_31_941 = mult(v_v__75_31_865, v__70);
            v_v__104_31_973 = mult(v__69[(64 + (7 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(64 + (7 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_31_941, v_v__104_31_973);
            v_v__101_32_942 = mult(v_v__75_32_866, v__70);
            v_v__104_32_974 = mult(v__69[(96 + (7 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))], v__71);
            v__106[(96 + (7 * v_N_5) + (64 * v_N_5 * v_wg_id_40) + (8 * v_N_5 * get_local_id(1)) + (128 * v_wg_id_41) + get_local_id(0))] = add(v_v__101_32_942, v_v__104_32_974);
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
