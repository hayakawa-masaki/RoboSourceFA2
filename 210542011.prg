1 ' ===================================
2 '
3 '  21054001 STEP5 Assy6�v���O����
4 '
5 ' �쐬�ҁF������T
6 ' �쐬���F2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1���痬�p
8 '
9 ' Ver 0.3 2021.12.17 �摜�����֐�ISInspection��ISInspectionSingle�A�摜�����ǉ� file:210542003
10 ' Ver 0.4 2022.03.06 �H��5�Ƃ̐i���֎~������
11 ' Ver 0.5 2022.04.08 BRACKET�����@�@�����҂� MSETTIMEOUT05&(5�b)�� MSETTIMEOUT08&(8�b)�ɕύX
12 ' ===================================
13 '===== <Insight�萔> =====
14 '===== <Insight�ϐ���`> =====
15 Dim PInspPosition(30)               '�摜����Function���n���p�ʒu�ϐ�
16 Dim MInspGroup%(30)                 '�摜����Function���n���p�ϐ�
17 Def Inte MIN_IS_Ready               '�y����IO�zInsight����OK
18 Def Inte MIN_IS_JobLoadOK           '�y����IO�zInsight�W���u���[�h����I��
19 Def Inte MIN_IS_JobLoadNG           '�y����IO�zInsight�W���u���[�h�ُ�I��
20 Def Inte MIN_IS_InspGSetOK          '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
21 Def Inte MIN_IS_InspGSetNG          '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
22 Def Inte MIN_IS_InspOK              '�y����IO�zInsight����OK
23 Def Inte MIN_IS_InspNG              '�y����IO�zInsight����NG
24 Def Inte MIN_IS_InspErr             '�y����IO�zInsight�����ُ�I��
25 Def Inte MIN_IS_InspCapDone         '�y����IO�zInsight�����摜�捞����
26 '
27 Def Inte MIN_IS_ErrNum              '�y����IO�zInsight�����G���[�ԍ��擾�J�n�A�h���X(16bit)
28 'Output Signal
29 Def Inte MOUT_IS_JobLoadReq         '�y�o��IO�zInsight JOB���[�h�v��
30 Def Inte MOUT_IS_InspGSetReq        '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
31 Def Inte MOUT_IS_Insp               '�y�o��IO�zInsight �������s�v��
32 '
33 Def Inte MOUT_IS_JobNum             '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
34 Def Inte MOUT_IS_InspGNum           '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
35 '
36 Def Inte MOUT_InspErrNum            '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
37 Def Inte MOUT_InspNGStepNum         '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
38 Def Inte MOUT_OKNG                 '
39 '��Ɨp�ϐ�
40 Def Inte MInspErrNum                '�������s�G���[�ԍ�
41 Def Inte MInspNGStepNum             '�������sNGStep�ԍ�
42 Def Inte MRtn                       'Function�߂�l�擾�p
43 Def Inte MRtn2                      'Function�߂�l�擾�p
44 Def Inte MRet3                      'Function�߂�l�擾�p
45 Def Inte MGRtn                      'Function�߂�l�擾�p �l�W�����@
46 Def Inte MInspErrNumSub             '�������s�G���[�ԍ�sub�@20190820�ǉ�
47 Def Inte MovrdA                     '�l�W����Ovrd �ϗp
48 Def Float MSpdA                     '�l�W����Spd�@�ϗp
49 Def Pos PTemp                       '�l�W���ߏ��ʒu�v�Z�p
50 Def Inte MKeyNum                    'KEY���͎󂯎��p(�ǉ�12/20����)
51 '===== <Insight�ϐ��ݒ�> =====
52 MIN_IS_Ready%        =   11380      '�y����IO�zInsight����OK
53 MIN_IS_JobLoadOK%    =   11381      '�y����IO�zInsight�W���u���[�h����I��
54 MIN_IS_JobLoadNG%    =   11382      '�y����IO�zInsight�W���u���[�h�ُ�I��
55 MIN_IS_InspGSetOK%   =   11383      '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
56 MIN_IS_InspGSetNG%   =   11384      '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
57 MIN_IS_InspOK%       =   11385      '�y����IO�zInsight����OK
58 MIN_IS_InspNG%       =   11386      '�y����IO�zInsight����NG
59 MIN_IS_InspErr%      =   11387      '�y����IO�zInsight�����ُ�I��
60 MIN_IS_InspCapDone%  =   11388      '�y����IO�zInsight�����摜�捞����
61 MIN_IS_ErrNum%       =   11408      '�y����IO�zInsight�����G���[�ԍ��J�n�A�h���X(16bit)
62 'Output Signal
63 MOUT_IS_JobLoadReq%  =   12370      '�y�o��IO�zInsight JOB���[�h�v��
64 MOUT_IS_InspGSetReq% =   12371      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
65 MOUT_IS_Insp%        =   12372      '�y�o��IO�zInsight �������s�v��
66 MOUT_IS_JobNum%      =   12384      '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
67 MOUT_IS_InspGNum%    =   12400      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
68 MOUT_InspErrNum%     =   12416      '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
69 MOUT_InspNGStepNum%  =   12432      '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
70 '===== <�d�h���ϐ���`> =====
71 X20_Driver=11248                    '�d�h���X�e�C�^�X1�@Driver Status 1
72 X21_Driver=11249 '�d�h���X�e�C�^�X2  Driver Status 2
73 X22_Driver=11250 '�d�h���X�e�C�^�X3  Driver Status 3
74 X23_Driver=11251 '�d�h���X�e�C�^�X4  Driver Status 4
75 X24_Driver=11252 '�d�h���G���[���b�Z�[�W1 Driver Error E1
76 X25_Driver=11253 '�d�h���G���[���b�Z�[�W2 Driver Error E2
77 X26_Driver=11254 '�d�h���G���[���b�Z�[�W3 Driver Error E3
78 X27_Driver=11255 '�d�h���G���[���b�Z�[�W4 Driver Error E4
79 X28_Driver=11256 '�d�h���g�[�^���G���[�V�O�i�� Total Error
80 X29_Driver=11257 '�d�h���I���V�O�i�� Comlete signal
81 X2A_Driver=11258 '�d�h���G���[���b�Z�[�W5 Driver Error E5
82 '11584   'toRB�g���N�h���C�o-COMP_ERR���M
83 Y60_Driver=12240 '�d�h�������v��� CCW
84 Y61_Driver=12241 '�d�h�����v��� CW
85 Y62_Driver=12242 '�o���N�Z�b�e�B���O BANK C1
86 Y63_Driver=12243 '�o���N�Z�b�e�B���O BANK C2
87 Y64_Driver=12244 '�o���N�Z�b�e�B���O BANK C3
88 Y65_Driver=12245 '�v���O�����Z�b�e�B���O PRG SET F1
89 Y66_Driver=12246 '�v���O�����Z�b�e�B���O PRG SET F2
90 Y67_Driver=12247 '�v���O�����Z�b�e�B���O PRG SET F3
91 X34_ScrewReady1=11259 '�˂�����1�@Read
92 '===== <�d�h���萔> =====
93 Dim PScrewPos(10)       '�l�W���ߗpFunction�����ϐ�
94 Dim PGetScrewPos(10)    '�˂������@����˂��𓾂�Function�����ϐ�
95 Dim PEscapePosi(10)
96 MLoopCnt% = 0'
97 '===== <���{�b�g�萔> =====
98 '===== <���{�b�g�ϐ���`> =====
99 MRBTOpeGroupNo = 0      '���{�b�g����ԍ�������
100 MCommentD1001 = 0
101 MCommentD1002 = 0
102 MCommentD1003 = 0
103 MScreenNo = 0
104 '
105 MCommentTSU = 0
106 MCommentTSD = 0
107 '�E�B���h��ʔԍ��ݒ�
108 MWindReSet = 0
109 MWindInfoScr = 5
110 MWindErrScr = 10
111 MWindErrScr2 = 11
112 MWindErrScr3 = 13
113 MWindErrScr17 = 17
114 MWindErrScr18 = 18
115 MWindCmmnScr = 20
116 MWindJigRelase19049 = 60
117 MWindJigRelase19050 = 61
118 MWindJigRelase19051 = 62
119 '
120 MClear% = 0        'KEY_�̃N���A
121 MAbout% = 1        'KEY_��~
122 MNext% = 2         'KEY_���̃X�e�b�v�ֈڍs
123 MContinue% = 3     'KEY_�p�� �ēx����������s��
124 '
125 MKeyNum% = 0       'KEY���͂��󂯎��
126 '
127 Def Inte MNgProcess
128 MNgProcess% = 5      'KEY_NG
129 '
130 MAssyOK% = 6       '�g������
131 MPass% = 7         '�H���p�X
132 MPiasNG% = 8       'Pias�m�F������NG
133 MIrregular% = 10   '��O�������s�p
134 '
135 '�������pKEY�ԍ�   '
136 MRobotInit1% = 11  '�����ʒu�p
137 MRobotInit2% = 12  '�����ʒu�p
138 MRobotInit3% = 13  '�����ʒu�p
139 MRobotInit4% = 14  '�����ʒu�p
140 '
141 MIN_INIT1REQUEST% = 11568 'toRBT_���{�b�g�����ʒu1�v��
142 MIN_INIT2REQUEST% = 11569 'toRBT_���{�b�g�����ʒu2�v��
143 MIN_INIT3REQUEST% = 11570 'toRBT_���{�b�g�����ʒu3�v��
144 MIN_INIT4REQUEST% = 11571 'toRBT_���{�b�g�����ʒu4�v��
145 '
146 MOUT_INIT1RECIVE% = 12560 'toPLC_���{�b�g�����ʒu1��M
147 MOUT_INIT2RECIVE% = 12561 'toPLC_���{�b�g�����ʒu2��M
148 MOUT_INIT3RECIVE% = 12562 'toPLC_���{�b�g�����ʒu3��M
149 MOUT_INIT4RECIVE% = 12563 'toPLC_���{�b�g�����ʒu4��M
150 '
151 MOK% = 1               '�e����p
152 MNG% = 0               '�e����p
153 MTIMEOUT% = -1         '�e����p
154 MJudge% = 0            '������i�[�p
155 '
156 MRECIVETIME& = 0
157 MSETTIMEOUT10& = 10000&                '10�b�ݒ�
158 MSETTIMEOUT08& = 8000&                 ' 8�b�ݒ�  2022/04/08 �ǉ�
159 MSETTIMEOUT03& = 3000&                 '3�b�ݒ�
160 MSETTIMEOUT01& = 1000&                 '1�b�ݒ�
161 MSETTIMEOUT05& = 5000&                 '5�b�ݒ�
162 MSETTIMEOUT009& = 900&                 '0.9�b�ݒ�
163 MSETTIMEOUT008& = 800&                 '0.8�b�ݒ�
164 MSETTIMEOUT007& = 700&                 '0.7�b�ݒ�
165 MSETTIMEOUT006& = 600&                 '0.6�b�ݒ�
166 MSETTIMEOUT005& = 500&                 '0.5�b�ݒ�
167 MSETTIMEOUT004& = 400&                 '0.4�b�ݒ�
168 MSETTIMEOUT003& = 300&                 '0.3�b�ݒ�
169 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
170 MIN_PIAS_ComOK% = 11552                'PC�ʐMOK
171 MIN_PIAS_ComTimeOut% = 11576           'PC�ʐM�m�F�^�C���A�E�g
172 MIN_PIAS_ComNG% = 11553                'PC�ʐMNG
173 MOUT_PIAS_ComCheck% = 12544            'PC�ʐM�m�F�v��
174 MOUT_PIAS_Missing_Process% = 12546     '�H�������m�F�v��
175 MIN_PIAS_ModelTypeNG% = 11554          '���f���d��NG
176 MIN_PIAS_ProcessHistryNG% = 11555      '�O�H������NG
177 MIN_PIAS_ProcessHistryOK% = 11556      '�O�H������OK
178 MIN_PIAS_ProcessHistryErr% = 11557     '�H�����������G���[
179 MIN_PIAS_MyProcessComp% = 11573        '���H����������
180 MIN_PIAS_ProcessHistryTimeOut% = 11578 '�H�������^�C���A�E�g
181 MOUT_OKNG% = 12226                     'PLC OUT ��OK=1, NG=0 �o��
182 '
183 MOUT_PiasPCBNumberCheck = 12557        '��ԍ��ƍ�
184 MIN_PiasPCBNumberOK% = 11566          '��ԍ�OK
185 MIN_PiasPCBNumberNG% = 11565          '��ԍ�NG
186 MIN_PiasPCBNumberErr% = 11567         '��ԍ������G���[
187 '
188 MOUT_PiasAssyResultOK% = 12549    '�g��OK
189 MOUT_PiasAssyResultNG% = 12550    '�g��NG
190 MOUT_PiasAssyResultWr% = 12548    '�H��������������
191 '
192 MIN_PiasProcessNG% = 11559        '�H����������NG
193 MIN_PiasProcessOtherErr% = 11560  '�H�����������G���[(�Ȃ񂩂̃g���u��)
194 MIN_PiasProcessOK% = 11558        '�H����������OK
195 '
196 MIN_Insight_Use% = 11374               '�摜�m�FON
197 MIN_TorqueCheck% = 11348               '�g���N�`�F�b�N
198 '
199 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT���쌠
200 MOUT_RED_LIGHT% = 12356            'PATLIGHT �� �_��
201 MOUT_RED_FLASH% = 12357            'PATLIGHT �� �_��
202 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT �� �_��
203 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT �� �_��
204 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT �� �_��
205 MOUT_GREEN_FLASH% = 12361          'PATLIGHT �� �_��
206 '
207 MOUT_ST_DATETIME% = 12551          '�g���J�n���t����
208 MOUT_ED_DATETIME% = 12552          '�g���I�����t����
209 '
210 MOUT_TORQUE_CHECK% = 12367         'PLC�փg���N�`�F�b�N���𑗐M
211 '
212 MIN_ASSY_CANCEL% = 11366           '�g�����s�����̃t���O
213 '
214 MLoopFlg% = 0                      'KEY���͌��OK or NG���e
215 MopeNo% = 0
216 MRtn% = 0
217 MRet = 0
218 MRet3% = 0
219 '
220 Def Inte MInputQty          '������ ���Z�ϐ�
221 Def Inte MAssyOkQty         '�g���n�j�� ���Z�ϐ�
222 Def Inte MAssyNgQty         '�g���m�f�� ���Z�ϐ�(���g�p)
223 Def Inte MSuctionErrQty     '�z���G���[�� ���Z�ϐ� 2022/04/27 �n��
224 Def Inte nAssyOkQty         '���g�p
225 Def Inte MScrewNo
226 Def Inte MReTry
227 '===== <IO�ϐ���`> =====
228 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
229 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
230 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
231 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
232 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
233 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
234 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
235 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
236 '
237 Def Inte Y6A_VV1            ' �A�[����[�@�l�W�z���o���u
238 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
239 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
240 '
241 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
242 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
243 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
244 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
245 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
246 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
247 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
248 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
249 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
250 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
251 '
252 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
253 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
254 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
255 '
256 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
257 '
258 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
259 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
260 '
261 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
262 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
263 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
264 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
265 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
266 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
267 '
268 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
269 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
270 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
271 '
272 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
273 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
274 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
275 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
276 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
277 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
278 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
279 Y6A_VV1%    =  12250    ' �A�[����[�@�l�W�z���o���u
280 Y6B_VB1%    =  12251    '�A�[����[�@�z���j��o���u
281 MOUT_VB1%   =  12251    ' �A�[����[�@�l�W�z���j��o���u
282 '
283 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
284 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
285 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
286 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
287 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
288 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
289 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
290 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
291 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
292 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
293 '
294 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
295 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
296 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
297 '
298 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
299 '
300 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
301 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
302 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
303 '
304 '����
305 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
306 Def Inte MOn                            '�o��=1
307 Def Inte MOff                           '�o��=0
308 '
309 '�˂����ߑ��u_�o�̓A�h���X
310 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
311 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
312 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
313 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
314 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
315 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
316 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
317 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
318 '�˂����ߑ��u_���̓A�h���X
319 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
320 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
321 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
322 Def Inte MIN_ScwT_Case1                 '����1��~����M
323 Def Inte MIN_ScwT_Case2                 '����2��~����M
324 Def Inte MIN_ScwT_Case3                 '����3��~����M
325 Def Inte MIN_ScwT_Case4                 '����4��~����M
326 Def Inte MIN_ScwT_Case5                 '����5��~����M
327 '
328 Dim MScwT_Case1%(2)               '����1��~�ϐ�
329 Dim MScwT_Case2%(2)               '����2��~�ϐ�
330 Dim MScwT_Case3%(2)               '����3��~�ϐ�
331 Dim MScwT_Case4%(2)               '����4��~�ϐ�
332 Dim MScwT_Case5%(2)               '����5��~�ϐ�
333 '
334 '����
335 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
336 MOn% = 1                                 '�o�� = 1
337 MOff% = 0                                '�o�� = 0
338 '
339 '�˂����ߋ@_�A�h���X�ݒ�
340 MOUT_ScwT_ComChk% = 12832               '�ʐM�m�F���M
341 MOUT_ScwT_ST% = 12865                   '�˂����ߊJ�n�𑗐M
342 MOUT_ScwT_ReSTOK% = 12866               '�ĊJ�n��M�𑗐M
343 MOUT_ScwT_FinOK% = 12868                '�˂����ߊ�����M�𑗐M
344 MOUT_ScwT_Case1OK% = 12858              '����1��~��M�𑗐M
345 MOUT_ScwT_Case2OK% = 12859              '����2��~��M�𑗐M
346 MOUT_ScwT_Case3OK% = 12860              '����3��~��M�𑗐M
347 MOUT_ScwT_Case4OK% = 12861              '����4��~��M�𑗐M
348 MOUT_ScwT_Case5OK% = 12862              '����5��~��M�𑗐M
349 '
350 MIN_ScwT_comOK% = 11840                 '�˂����ߑ��u����ԐM
351 MIN_ScwT_STRec% = 11873                 '�˂����ߊJ�n����M
352 MIN_ScwT_ReST% = 11874                  '�ĊJ�n����M
353 MIN_ScwT_Fin% = 11876                   '�˂����ߊ�������M
354 MIN_ScwT_Case1% = 11866                 '����1��~�ҋ@����M
355 MIN_ScwT_Case2% = 11867                 '����2��~�ҋ@����M
356 MIN_ScwT_Case3% = 11868                 '����3��~�ҋ@����M
357 MIN_ScwT_Case4% = 11869                 '����4��~�ҋ@����M
358 MIN_ScwT_Case5% = 11870                 '����5��~�ҋ@����M
359 '
360 MScwT_Case1%(1) = MIN_ScwT_Case1%
361 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
362 MScwT_Case2%(1) = MIN_ScwT_Case2%
363 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
364 MScwT_Case3%(1) = MIN_ScwT_Case3%
365 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
366 MScwT_Case4%(1) = MIN_ScwT_Case4%
367 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
368 MScwT_Case5%(1) = MIN_ScwT_Case5%
369 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
370 '
371 '�ݒ� InitialZoneB�Ŏg�p����ϐ�
372 Def Pos PActive       '�������W�n �ʒu�ϐ� ���݈ʒu
373 Def Pos Pmove         '�������W�n �ʒu�ϐ� �ړ���
374 Def Jnt JActive       '�֐ߍ��W�n �ʒu�ϐ� ���݈ʒu
375 Def Jnt Jmove         '�֐ߍ��W�n �ʒu�ϐ� �ړ���
376 Def Jnt JTaihi        '�֐ߍ��W�n �ʒu�ϐ� �ޔ��|�W�V���� �e�B�[�`���O�Őݒ�
377 Def Inte MRecoveryPass      '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s
378 Def Inte MStandby      '�ҋ@�ʒu�m�F�t���O
379 Def Inte MRecoveryChuckOpen    '�`���b�N����t���O�i���A����O�j���͂ݑ΍�
380 '�����Ӂ������ʒu��ύX�������ɂ́A�ύX���K�v�I
381 '
382 '
383 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
384 Function M% fnAssyStart
385     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
386 ' PIAS�`�P�b�g�Ǎ��ݍH�������m�F
387     M_20# = MClear%                       '������
388     If M_22# = MIrregular% Then GoTo *IRREGULAR     'Assy������DVD���J��c�����Ă���ꍇ
389     '�J�n�ʒu���C�j�V�����|�W�V�����������ꍇ(�n���h��������������`�F�b�N)
390     *RE_START
391     PTemp = P_Curr
392     MRtn = 0
393     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
394         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
395             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
396                 MRtn = 1
397             EndIf
398         EndIf
399     EndIf
400     '�C�j�V�����|�W�V�����ɂ����ꍇ�Z���T�[�`�F�b�N
401     If MRtn = 1 Then
402         Ovrd 20
403         Accel 100 , 20
404         Mvs PHandChange                             '�n���h�����ʒu�Ɉړ�
405         Ovrd 100
406         Accel 100 , 100
407         MRtn = frInCheck(11264,0,MSETTIMEOUT03&)    '�Z���T�[�`�F�b�N(���������)
408         Mvs PInitialPosition                        '�C�j�V�����|�W�V�����Ɉړ�
409     Else
410         MRtn = 1
411     EndIf
412     If MRtn = 1 Then GoTo *AssyStart
413     fErrorProcess(11,286,287,0)                        '�n���h�����p�������
414     If M_20# = MNext% Then M_20# = MClear%
415     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
416     If M_20# = MNgProcess% Then GoTo *RE_START
417     If M_20# = MContinue% Then GoTo *RE_START
418 '
419     *AssyStart
420 '
421 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
422 '        MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
423 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
424 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
425 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' ���j���[�֖߂�
426 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NG���H�������ɏ����ݎ��̍H����
427 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NG���H�������ɏ����ݎ��̍H����
428 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' ����NG, �H������
429 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK�ȊO�͑g���I��
430 '    EndIf
431 ' �l�W���ߋ@�e�X�g�p ----------
432 '    'Mret% = fScewTcomChk()
433 '    '�˂����ߊJ�n
434     MRtn2 = fScewTStart()
435 '    '
436 '    '���W�ړ�
437 '    '
438 '    '����xx��~
439 '    fScewTCaseStop(MScwT_Case5%)
440 '    '
441 '    '�x�[�X���j�b�gKEY
442 '    Wait M_In(MTEST_KEY%) = MOn%
443 '    '
444 '    '�ĊJ�n
445 '    fScewTReStart()
446 '    '
447 '    '���W�ړ�
448 '    '
449 '    '�˂����ߊ���
450 '    Mret% = fScewTFinish()
451 ' �l�W���߃e�X�g�I��
452 ' PIAS�e�X�g -----------
453 '    MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
454 '    MRet% = fnPiasWrite(MNG%)
455  '   MRet% = fnPCBNumberCheck()     '�f�o�b�N�p�ɃR�����g�A�E�g(9/17����)
456 ' PIAS�e�X�g�I�� -------
457 '�g�ݗ��ĊJ�n(���g��9/10����)
458 '�v���O�������_
459 Ovrd 100
460     If MRtn2 = 0 Then GoTo *INITIAL_CHECK
461     fErrorProcess(11,329,201,0)
462 If M_20# = MNext% Then GoTo *AssyStart
463 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
464 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
465 If M_20# = MContinue% Then GoTo *AssyStart
466 '
467 '
468 '
469 '
470 '
471 '
472 '
473 '�n���h��DVD���J,�u���P�b�g��������
474 '
475 *INITIAL_CHECK
476 '
477 If M_In(11264) = 0 And M_In(11267) = 0 And M_In(11270) = 0 Then GoTo *CompInitial1  'DVD���J,�u���P�b�g��������
478 fErrorProcess(11,253,287,0)                 '284��287�ɕύX6/3����
479 If M_20# = MNext% Then M_20# = MClear%
480 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
481 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
482 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
483 *CompInitial1
484 '
485 '�n���h���C�j�V�����ɖ߂�
486 If M_In(11266) = 1 Then     'DVD�`���b�N���o
487     M_Out(12256) = 0        'DVD�`���b�N��OFF
488     M_Out(12257) = 1        'DVD�`���b�N�JON
489     Break
490 EndIf
491 If M_In(11269) = 1 Then     'F�V�����_�[�o���o
492     M_Out(12258) = 0        'F�V�����_�[�oOFF
493     M_Out(12259) = 1        'F�V�����_�[��ON
494     Break
495 EndIf
496 If M_In(11272) = 1 Then     'R�V�����_�[�o���o
497     M_Out(12260) = 0        'R�V�����_�[�oOFF
498     M_Out(12261) = 1        'R�V�����_�[��ON
499     Break
500 EndIf
501 '
502 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    'DVD�`���b�N�J���o
503 If MRtn = 1 Then GoTo *CompInitial2
504 fErrorProcess(11,270,284,0)
505 If M_20# = MNext% Then M_20# = MClear%
506 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
507 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
508 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
509 *CompInitial2
510 '
511 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)    'F�V�����_�[�ߌ��o
512 If MRtn = 1 Then GoTo *CompInitial3
513 fErrorProcess(11,278,284,0)
514 If M_20# = MNext% Then M_20# = MClear%
515 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
516 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
517 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
518 *CompInitial3
519 '
520 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)    'R�V�����_�[�ߌ��o
521 If MRtn = 1 Then GoTo *CompInitial4
522 fErrorProcess(11,276,284,0)
523 If M_20# = MNext% Then M_20# = MClear%
524 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
525 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
526 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
527 *CompInitial4
528 '
529 ' 2022/04/11 ���S�����֏����ǉ� �n��
530 ' PInitialPosition �ݐ� MStandby=1
531 ' PMechaOnJigGet_3 �ݐ� MStandby=2
532 '
533 MStandby = 0    '�ҋ@�ʒu�t���O��������
534 PTemp = P_Curr
535 If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
536     If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
537         If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
538             MStandby = 1
539         EndIf
540     EndIf
541 EndIf
542 If (PTemp.X <= PMechaOnJigGet_3.X + 1.0) And (PTemp.X >= PMechaOnJigGet_3.X - 1.0) Then
543     If ((PTemp.Y <= PMechaOnJigGet_3.Y + 1.0) And (PTemp.Y >= PMechaOnJigGet_3.Y - 1.0)) Then
544         If ((PTemp.Z <= PMechaOnJigGet_3.Z + 1.0) And (PTemp.Z >= PMechaOnJigGet_3.Z - 1.0)) Then
545             MStandby = 2
546         EndIf
547     EndIf
548 EndIf
549 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
550     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
551         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
552             MStandby = 3
553         EndIf
554     EndIf
555 EndIf
556 If MStandby <> 0 Then GoTo *PositionOK
557 fErrorProcess(11,230,281,0)          '�����ʒu�ɂ��Ȃ����̓G���[�ɂ���
558 If M_20# = MNext% Then GoTo *ASSY_ERROR_END
559 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
560 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
561 If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
562 *PositionOK
563 '
564 '
565 'DVD����PASS�v���O��������ʏ�v���O�����ɐؑւ������̑΍� 2022.05.13 �n��
566 PTemp = P_Curr
567 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
568     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
569         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
570             Ovrd 50
571             Mov PInitialPosition
572             Ovrd 100
573         EndIf
574     EndIf
575 EndIf
576 '
577 '
578 M_Out(12912) = 1                  'Ver 0.4 �ǉ��@���u����t���O����(�H��6�D��̂��ߍH��5�̃t���O�Ď��̑O�ɏo��)
579 Mov PMechaOnJigGet_3    '�^�N�g�Z�k�̂��ߏ����ʒu�ύX
580 'Wait M_In(11920) = 0              'BaseUnit5���u����t���O�m�F(�����ʒu�ړ�2/9����)
581 M_Out(12912) = 1                  '���u����t���O����(�O�̂��ߒǉ�2/9����)
582 'Ver 0.4 �ǉ�--------------------
583 If M_In(11920) = 1 Then GoTo *CompInitial4         '�H��6�����쒆�̏ꍇ11920 = 1 ���[�v
584 'Ver 0.4 �����܂�----------------           '�H��6�D��̂���12912=0�ɂ��Ȃ�
585 '
586 'Mov PInitialPosition   '1/20�R�����g�A�E�g(����)
587 '
588 '���u������_�ֈړ�
589 Mov PMechaOnJigGet_2         '���u������_
590 '
591 '���u���䂩��DVD���J���󂯎��
592 'Wait M_In(12914) = 1              '����������M
593 'Wait M_In(11920) = 0              'BaseUnit5���u����t���O�m�F(�ǉ���������10/1����)
594 '
595     MRtn2 = 0                   '�G���[�����ւ������ꂽ���p
596 *RE_JIG_GET
597     M_20# = MClear%
598 '
599     M_Out(12912) = 1                  '���u����t���O����
600     If M_In(11278) = 1 Then          '�����̊m�F(CW�[�Z���T�[)(�ǉ������܂�10/1����)
601         Mov PMechaOnJigGet1_1    '���u������
602         Ovrd 25
603         Mvs PMechaOnJigGet1      'DVD���J�󂯎��ʒu
604         M_Out(12257) = 0         'DVD���J�`���b�N�JOFF
605         M_Out(12256) = 1         'DVD���J�`���b�N��ON
606 '        Wait M_In(11266) = 1     'DVD���J�`���b�N���o
607         MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
608         If MRtn = 0 And MRtn2 = 0 Then             'DVD���J��c���ł��Ȃ������ꍇ�`���b�N���J��(6/3����)
609             M_Out(12256) = 0         'DVD���J�`���b�N��OFF
610             M_Out(12257) = 1         'DVD���J�`���b�N�JON
611         EndIf
612         Mvs PMechaOnJigGet1_1    '���u������
613         Ovrd 100
614         Break
615     ElseIf M_In(11277) = 1 Then       '�����̊m�F(CCW�[�Z���T�[)(�ǉ���������10/1����)
616         Mov PMechaOnJigGet2_1    '���u������
617         Ovrd 25
618         Mvs PMechaOnJigGet2      'DVD���J�󂯎��ʒu
619         M_Out(12257) = 0         'DVD���J�`���b�N�JOFF
620         M_Out(12256) = 1         'DVD���J�`���b�N��ON
621 '        Wait M_In(11266) = 1     'DVD���J�`���b�N���o
622         MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
623         If MRtn = 0 And MRtn2 = 0 Then             'DVD���J��c���ł��Ȃ������ꍇ�`���b�N���J��(6/3����)
624             M_Out(12256) = 0         'DVD���J�`���b�N��OFF
625             M_Out(12257) = 1         'DVD���J�`���b�N�JON
626         EndIf
627         Mvs PMechaOnJigGet2_1    '���u������
628         Ovrd 100
629         Break
630     EndIf
631     '
632     If MRtn = 1 Or MRtn2 = 1 Then GoTo *CompMechaGet1    'DVD���J�`���b�N�G���[����(Or MRtn2 = 1�ǉ�6/3����)
633 '    PTemp = P_Curr              '�����ʒu�̕ύX1/12����
634 '    Mvs PTemp , -150
635     Mov PMechaOnJigGet_2
636     fErrorProcess(11,269,294,0) '284��294�ɕύX6/3����
637 '    PTemp = P_Curr             '�����ʒu�̕ύX1/12����
638 '    Mvs PTemp , -150
639 '    Mov PMechaOnJigGet_2
640     If M_20# = MNext% Then
641         M_20# = MClear%
642         MRtn2 = 1           '�ǉ�6/3����
643     EndIf
644     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ǉ�6/3����
645         Mov PInitialPosition
646         Break
647     EndIf
648     If M_20# = MContinue% Then MRtn2 = 0
649     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition '�ǉ�6/3����
650     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
651     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
652     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_JIG_GET  '���ւ������ꂽ�ꍇ���߂�悤�ɕύX(6/3����)
653 *CompMechaGet1
654 '
655     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
656 Mov PMechaOnJigGet_2            '���u������_
657     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
658 '
659 'DVD���J�Z���T�[���o
660 '
661 'M_Out(12912) = 0                  '���u����t���O���(�����ʒu�ύX2/9����)
662 '
663 '
664 'DVD���J���˂����{3�ɒu��
665 Mov PMechaOnRoboSet_2        '�˂����{���_
666 '
667 'Wait M_In(11888) = 1         '�˂����{3��~1�܂őҋ@
668 MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
669 If MRtn = 0 Then Mov PInitialPosition     '"�C�j�V�����ɖ߂铮��"
670 If MRtn = 0 Then GoTo *ASSY_ERROR_END
671 '
672 Mov PMechaOnRoboSet_1        '�˂����{���
673 Ovrd 25
674 Mvs PMechaOnRoboSet          'DVD���J�u����
675 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~1�`��~2�܂�)
676 '
677 'Wait M_In(11889) = 1         '�˂����{3��~2�܂őҋ@
678 MRtn = fScrewTighenRoboCheck(11889)    '��~��Ԃ���M����
679 'If MRtn = 0 Then
680 '    M_Out(12256) = 0             'DVD���J�`���b�N��OFF
681 '    M_Out(12257) = 1             'DVD���J�`���b�N�JON
682 '    Mvs PMechaOnRoboSet_1
683 '    Mov PMechaOnRoboSet_2
684 '    Mov PInitialPosition
685 'EndIf
686 If MRtn = 0 Then GoTo *ASSY_ERROR_END   '���̏�Œ�~����(�̏�̉\��)
687 '
688 *RE_ROBO_SET_1
689 '
690 M_Out(12256) = 0             'DVD���J�`���b�N��OFF
691 M_Out(12257) = 1             'DVD���J�`���b�N�JON
692 '
693 'Wait M_In(11265) = 1         'DVD���J�`���b�N�J���o
694 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
695 If MRtn = 1 Then GoTo *CompRoboSet1
696 fErrorProcess(11,270,284,0)
697 If M_20# = MNext% Then M_20# = MClear%
698 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
699 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
700 If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
701 *CompRoboSet1
702 '
703 '    ' ���i�����v�����M(�����ʒu�ύX1/20����)
704     M_Out(12787) = 1
705 '
706 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~2�`��~3�܂�)
707 '
708 'Wait M_In(11890) = 1         '�˂����{3��~3�܂őҋ@
709 MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����
710 MScrewRoboNgFlg% = 0
711 If MRtn = 0 Then MScrewRoboNgFlg% = 1
712 '
713 Mvs PMechaOnRoboSet_1        '�˂����{���
714 Ovrd 100
715 Mov PMechaOnRoboSet_2        '�˂����{���_
716 M_Out(12912) = 0                  '���u����t���O���(�����ʒu�ύX2/9����)
717 '
718 '
719 If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition   '�˂����{�Œ�~��NG��������Ă����ꍇ
720 If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END   '
721 '
722 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~3�`��~4�܂�)
723 '
724 ''    ' ���i�����v�����M(�����ʒu�ʒu�ύX1/20����)
725 '    M_Out(12787) = 1
726 '    '    ' ���i���������҂�
727 '    Wait M_In(11810) = 1
728 '
729 'DVD����(R)�����
730 *RE_R_GET_3
731 Mov PBracketRGet_3           '�o�H
732 Mov PBracketRGet_2           '����(R)�󂯎����_
733 M_Out(12261) = 0             '����(R)�V�����_�[��OFF
734 M_Out(12260) = 1             '����(R)�V�����_�[�oON
735 '
736     '    ' ���i���������҂�(�����ύX2/27����)
737 *RE_FEEDER_READY
738 '    Wait M_In(11810) = 1
739     fnAutoScreenComment(513)    '��ԕ\��[���i�����҂�] 2022/04/26 �n��
740 'MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '�����҂�
741 MRtn = frInCheck(11810,1,MSETTIMEOUT08&)   '�����҂�  2022/04/07 �ύX
742 If MRtn = 1 Then GoTo *CompFeederReady
743 '    ' ���i�����v���I��
744     M_Out(12787) = 0
745 fErrorProcess(11,289,284,0)
746 If M_20# = MNext% Then M_20# = MClear%
747 If M_20# = MAbout% Or M_20# = MNgProcess% Then
748     Mov PBracketRGet_2
749     Mov PBracketRGet_3
750     Mov PBracketRSet_3
751     Mov PInitialPosition1
752 EndIf
753 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
754 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
755 '    ' ���i�����v��
756     M_Out(12787) = 1
757 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
758 *CompFeederReady
759 '    ' ���i�����v���I��
760     M_Out(12787) = 0
761 '
762     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
763 Mov PBracketRGet_1           '����(R)�󂯎����
764 '
765 *RE_R_GET_1
766 '
767 If M_20# = MContinue% Then
768     M_Out(12261) = 0             '����(R)�V�����_�[��OFF
769     M_Out(12260) = 1             '����(R)�V�����_�[�oON
770     M_20# = MClear%
771 EndIf
772 '
773 'Wait M_In(11272) = 1         '����(R)�V�����_�[�o�[���o
774 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '����(R)�V�����_�[�o�[���o
775 If MRtn = 1 Then GoTo *CompRGet1
776 fErrorProcess(11,275,284,0)
777 If M_20# = MNext% Then M_20# = MClear%
778 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
779 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
780 If M_20# = MContinue% Then GoTo *RE_R_GET_1
781 *CompRGet1
782 '
783 Ovrd 25
784 '
785 *RE_R_GET_2
786 '
787 Mvs PBracketRGet             '����(R)�󂯎��ʒu
788 '
789 '
790 M_Out(12252) = 0             '�^��OFF�o���uOFF
791 M_Out(12253) = 0             '�^��j��o���uOFF(�O�̂���)
792 M_Out(12251) = 1             '�^��ON�o���uON
793 '
794 'Wait M_In(11270) = 1         '����(R)�z���Z���T�[ON���o
795 MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   '����(R)�z���Z���T�[ON���o
796 If MRtn = 1 Then GoTo *CompRGet2
797 Mvs PBracketRGet_1           '����(R)�󂯎����
798 fErrorProcess(11,279,295,0)  '284��295�֕ύX6/3����
799 If M_20# = MNext% Then M_20# = MClear%
800 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
801 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
802 If M_20# = MContinue% Then GoTo *RE_R_GET_2
803 *CompRGet2
804 '
805 Mvs PBracketRGet_1           '����(R)�󂯎����
806 Ovrd 100
807 Mov PBracketRGet_2           '����(R)�󂯎����_
808 Mov PBracketRGet_3           '�o�H
809 '
810 'DVD����(R)��u��
811 Mov PBracketRSet_3           '���_
812 MRtn = frInCheck(11270,1,MSETTIMEOUT05&)              '������x����(R)�z���Z���T�[ON���o
813 If MRtn = 1 Then GoTo *CompRGet3
814 fErrorProcess(11,279,295,0)  '284��295�֕ύX6/3����
815 If M_20# = MNext% Then M_20# = MClear%
816 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
817 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
818 If M_20# = MContinue% Then GoTo *RE_R_GET_3
819 *CompRGet3
820 '
821 'Wait M_In(11891) = 1         '�˂����{3��~4�܂őҋ@
822 MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
823 If MRtn = 0 Then Mov PInitialPosition
824 If MRtn = 0 Then GoTo *ASSY_ERROR_END
825 '
826 Mov PBracketRSet_2           '����(R)�u���ʒu���_
827 Mvs PBracketRSet_1           '����(R)�u���ʒu���
828 *RE_R_SET_1                  '�߂��ʒu�ύX6/3����
829 Ovrd 10
830 Mvs PBracketRSet             '����(R)�u���ʒu
831 Dly 0.2
832 '
833 '*RE_R_SET_1                  '�߂��ʒu�ύX6/3����
834 '
835 M_Out(12251) = 0             '�^��ON�o���uOFF
836 M_Out(12252) = 1             '�^��OFF�o���uON
837 M_Out(12253) = 1             '�^��j��o���uON
838 '
839 MRtn = frInCheck(11270,0,MSETTIMEOUT05&)
840 '
841 Dly 0.5
842 'M_Out(12253) = 0             '�^��j��o���uOFF
843 '
844 If MRtn = 1 Then GoTo *CompRSet1
845 Mvs PBracketRSet_1           '����(R)�u���ʒu���
846 M_Out(12253) = 0             '�^��j��o���uOFF
847 fErrorProcess(11,296,297,0)  '236,284��296,297�ɕύX6/2����
848 If M_20# = MNext% Then M_20# = MClear%
849 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
850 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
851 If M_20# = MContinue% Then GoTo *RE_R_SET_1
852 *CompRSet1
853 '
854 'M_Out(12260) = 0             '����(R)�V�����_�[�oOFF(�߂��ʒu�ύX1/20����)
855 'M_Out(12261) = 1             '����(R)�V�����_�[��ON
856 ''
857 '*RE_R_SET_2
858 ''
859 ''Wait M_In(11271) = 1         '����(R)�V�����_�[�ߒ[���o
860 'MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   '����(R)�V�����_�[�ߒ[���o
861 'If MRtn = 1 Then GoTo *CompRSet2
862 'fErrorProcess(11,276,284,0)
863 'If M_20# = MNext% Then M_20# = MClear%
864 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
865 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
866 'If M_20# = MContinue% Then GoTo *RE_R_SET_2
867 '*CompRSet2
868 '
869 Ovrd 1                       '����������h�~(5/13����)
870 Mvs PBracketRSet , -5        '����������h�~(5/13����)
871 Ovrd 100                     '����������h�~(5/13����)
872 Mvs PBracketRSet_1           '����(R)�u���ʒu���
873 M_Out(12253) = 0             '�^��j��o���uOFF
874 Ovrd 100
875 Mov PBracketRSet_2           '����(R)�u���ʒu���_
876 Mov PBracketRSet_3           '���_
877 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~4�`��~5�܂�)
878 '
879 *RE_R_SET_2
880 '
881 M_Out(12260) = 0             '����(R)�V�����_�[�oOFF(�߂��ʒu�ύX1/20����)
882 M_Out(12261) = 1             '����(R)�V�����_�[��ON
883 '
884 'Wait M_In(11271) = 1         '����(R)�V�����_�[�ߒ[���o
885 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   '����(R)�V�����_�[�ߒ[���o
886 If MRtn = 1 Then GoTo *CompRSet2
887 fErrorProcess(11,276,284,0)
888 If M_20# = MNext% Then M_20# = MClear%
889 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
890 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
891 If M_20# = MContinue% Then GoTo *RE_R_SET_2
892 *CompRSet2
893 '
894 '
895 '�u���ʒu�摜����(�����ʒu�ړ�)
896 'Wait M_In(11892) = 1         '�˂����{3��~5�܂őҋ@(�摜��������F���u���P�b�g�u��)
897 'MRtn = fScrewTighenRoboCheck(11892)    '��~��Ԃ���M����
898 'If MRtn = 0 Then Mov PInitialPosition
899 'If MRtn = 0 Then GoTo *ASSY_ERROR_END
900 '
901 'Wait M_In(11920) = 0              'BaseUnit5���u����t���O�m�F(�ǉ���������10/1����)
902 ''
903 'M_Out(12912) = 1                  '���u����t���O����(�Փ˖h�~)
904 ''
905 ''Mov PBracketRCheck_2         '�o�H
906 ''Mov PBracketRCheck           '�����ʒu
907 ''
908 '*RE_R_CHECK
909 ''If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck1
910 'PInspPosition(1) = PBracketRCheck1
911 'MInspGroup%(1) = 2
912 'PInspPosition(2) = PBracketRCheck2
913 'MInspGroup%(2) = 3
914 'MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
915 'If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck1
916 'If MRtn = 0 Then
917 '    MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
918 'EndIf
919 'If MRtn = 1 Then GoTo *CompRCheck
920 'fErrorProcess(11,43,46,0)
921 'If M_20# = MNext% Then M_20# = MClear%
922 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
923 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
924 'If M_20# = MContinue% Then GoTo *RE_R_CHECK
925 '*CompRCheck
926 '*SkipCheck1
927 '
928 'M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~5�`��~6�܂�)
929 '
930 'Mov PBracketRCheck
931 '
932 'DVD����(F)�����
933 *RE_F_GET_3
934 'M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~5�`��~6�܂�)(�����ʒu�ύX1/20����)
935 Mov PBracketFGet_3           '���_
936 '
937 M_Out(12912) = 0                  '���u����t���O���(�Փ˖h�~)
938 '
939 Mov PBracketFGet_2           '����(F)�󂯎����_
940 'Mov PBracketFGet_1           '����(F)�󂯎����(�ړ��^�C�~���O�ύX1/20����)
941 '
942 *RE_F_GET_1
943 '
944 M_Out(12259) = 0             '����(F)�V�����_�[��OFF
945 M_Out(12258) = 1             '����(F)�V�����_�[�oON
946 '
947 Mov PBracketFGet_1           '����(F)�󂯎����(�ړ��^�C�~���O�ύX1/20����)
948 '
949 'Wait M_In(11269) = 1         '����(F)�V�����_�[�o�[���o
950     fnAutoScreenComment(513)    '��ԕ\��[���i�����҂�] 2022/04/26 �n��
951 MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   '����(F)�V�����_�[�o�[���o
952 If MRtn = 1 Then GoTo *CompFGet1
953 fErrorProcess(11,277,284,0)
954 If M_20# = MNext% Then M_20# = MClear%
955 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
956 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
957 If M_20# = MContinue% Then GoTo *RE_F_GET_1
958 *CompFGet1
959 '
960 Ovrd 25
961 *RE_F_GET_2
962     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
963 Mvs PBracketFGet             '����(F)�󂯎��ʒu
964 '
965 '
966 '
967 M_Out(12249) = 0             '�^��OFF�o���uOFF
968 M_Out(12250) = 0             '�^��j��o���uOFF
969 M_Out(12248) = 1             '�^��ON�o���uON
970 '
971 'Wait M_In(11267) = 1         '����(F)�z���Z���T�[ON���o
972 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '����(F)�z���Z���T�[ON���o
973 If MRtn = 1 Then GoTo *CompFGet2
974 Mvs PBracketFGet_1           '����(F)�󂯎����
975 fErrorProcess(11,280,295,0)  '284��295�ɕύX6/3����
976 If M_20# = MNext% Then M_20# = MClear%
977 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
978 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
979 If M_20# = MContinue% Then GoTo *RE_F_GET_2
980 *CompFGet2
981 '
982 Mvs PBracketFGet_1           '����(F)�󂯎����
983 'Ovrd 100
984 Mov PBracketFGet_2           '����(F)�󂯎����_
985 Mov PBracketFGet_3           '���_
986 '
987 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)          '������x����(F)�z���Z���T�[ON���o
988 If MRtn = 1 Then GoTo *CompFGet3
989 fErrorProcess(11,280,295,0)  '284��295�ɕύX6/3����
990 If M_20# = MNext% Then M_20# = MClear%
991 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
992 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
993 If M_20# = MContinue% Then GoTo *RE_F_GET_3
994 *CompFGet3
995 '
996 'DVD����(F)��u��
997 Mov PBracketFSet_3           '���_
998 '    ' ���i�����v���I��
999     M_Out(12787) = 0
1000 '    ' ���i�擾�������M(�p���X)
1001     M_Out(12800) = 1 Dly 0.5
1002     '
1003 Ovrd 70
1004 Mov PBracketFSet_2           '����(F)�u���ʒu���_
1005 'Wait M_In(11893) = 1         '�˂����{3��~5�܂őҋ@
1006 MRtn = fScrewTighenRoboCheck(11892)    '��~��Ԃ���M����
1007 If MRtn = 0 Then Mov PInitialPosition1  '��~�ʒu�Ɉړ�
1008 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1009 '
1010 '    fnAutoScreenComment(533)    '��ԕ\��[�H���T�̃��{����I���҂�] 2022/04/26 �n��(�R�����g�A�E�g5/13����)
1011 'Wait M_In(11920) = 0              'BaseUnit5���u����t���O�m�F(�ǉ���������10/1����)(�R�����g�A�E�g5/13����)
1012 ''
1013 'M_Out(12912) = 1                  '���u����t���O����(�Փ˖h�~)(�R�����g�A�E�g5/13����)
1014 ''
1015     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1016 'Ovrd 70
1017 'Mov PBracketFSet_2           '����(F)�u���ʒu���_
1018 Mvs PBracketFSet_1           '����(F)�u���ʒu���
1019 *RE_F_SET_1                  '�߂��ʒu�ύX6/3����
1020 Ovrd 25
1021 Mvs PBracketFSet             '����(F)�u���ʒu
1022 Dly 0.1
1023 '
1024 '*RE_F_SET_1                  '�߂��ʒu�ύX6/3����
1025 '
1026 M_Out(12248) = 0             '�^��ON�o���uOFF
1027 M_Out(12249) = 1             '�^��OFF�o���uON
1028 M_Out(12250) = 1             '�^��j��o���uON
1029 '
1030 MRtn = frInCheck(11267,0,MSETTIMEOUT05&)
1031 Dly 0.5
1032 'M_Out(12250) = 0             '�^��j��o���uOFF
1033 '
1034 If MRtn = 1 Then GoTo *CompFSet1
1035 M_Out(12250) = 0             '�^��j��o���uOFF
1036 fErrorProcess(11,296,297,0)  '236,284��296,297�ɕύX6/2����
1037 If M_20# = MNext% Then M_20# = MClear%
1038 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1039 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1040 If M_20# = MContinue% Then GoTo *RE_F_SET_1
1041 *CompFSet1
1042 '
1043 '
1044 '
1045 '*RE_F_SET_2'(�߂��ʒu�ύX)
1046 ''
1047 'M_Out(12258) = 0             '����(F)�V�����_�[�oOFF
1048 'M_Out(12259) = 1             '����(F)�V�����_�[��ON
1049 ''
1050 ''Wait M_In(11268) = 1         '����(F)�V�����_�[�ߒ[���o
1051 'MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '����(F)�V�����_�[�ߒ[���o
1052 'If MRtn = 1 Then GoTo *CompFSet2
1053 'fErrorProcess(11,277,284,0)
1054 'If M_20# = MNext% Then M_20# = MClear%
1055 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1056 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1057 'If M_20# = MContinue% Then GoTo *RE_F_SET_2
1058 '*CompFSet2
1059 '
1060 Ovrd 1                       '�u���P�b�g����������h�~(5/12����)
1061 Mvs PBracketFSet , -5        '�u���P�b�g����������h�~(5/12����)
1062 Ovrd 100
1063 Mvs PBracketFSet_1           '����(F)�u���ʒu���
1064 M_Out(12250) = 0             '�^��j��o���uOFF
1065 Mvs PBracketFSet_2           '����(F)�u���ʒu���_
1066 Mov PBracketFSet_3           '���_
1067 M_Out(12912) = 0             '���u����t���O���
1068 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~5�`��~6�܂�)
1069 *RE_F_SET_2'(�߂��ʒu�ύX)
1070 '
1071 M_Out(12258) = 0             '����(F)�V�����_�[�oOFF
1072 M_Out(12259) = 1             '����(F)�V�����_�[��ON
1073 '
1074 'Wait M_In(11268) = 1         '����(F)�V�����_�[�ߒ[���o
1075 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '����(F)�V�����_�[�ߒ[���o
1076 If MRtn = 1 Then GoTo *CompFSet2
1077 fErrorProcess(11,277,284,0)
1078 If M_20# = MNext% Then M_20# = MClear%
1079 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1080 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1081 If M_20# = MContinue% Then GoTo *RE_F_SET_2
1082 *CompFSet2
1083 '
1084 '----------PIAS��ǂ�----------
1085 '�T�v�F�ǂ݂ɍs�����ǂ��������߂�Ƃ��Ƀ^�C���A�E�g�Ő�ɐi�߂�悤�ɂ���^�C�}�[
1086     Mov PTicketRead_2
1087     Mov PTicketRead_1
1088     M_22# = MClear%
1089     M_20# = MClear%
1090     M_Timer(4) = 0
1091     MloopFlg = 0
1092     While MloopFlg = 0
1093         MCrtTime& = M_Timer(4)
1094         If M_In(11354) = 1 Then
1095             MloopFlg = 1
1096         ElseIf MCrtTime& > 5000 Then    '�b��5�b(�������Ă���͕̂b���������₷�����Ă����)
1097             MloopFlg = 1
1098         EndIf
1099     WEnd
1100     MRtn = 0
1101     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
1102         If M_In(11354) = 1 Then         '�g���J�n��ON�Ȃ�
1103             M_Out(12346) = 1 Dly 0.5        ' �g���J�n����M
1104             Mvs PTicketRead
1105             MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
1106 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1107 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1108             If MRtn = 1 Then M_22# = MAssyOK%
1109             If M_20# = MContinue% Then M_22# = MContinue%
1110             If M_20# = MPass% Then M_22# = MPass%
1111             If M_20# = MNext% Then M_22# = MPass%
1112         EndIf
1113     EndIf
1114     If M_20# = MNgProcess% Or M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1115     M_20# = MClear%
1116     Mov PTicketRead_1
1117     Mov PTicketRead_2
1118     Mov PBracketFSet_3
1119 '
1120 '�u���ʒu�摜����
1121 'Wait M_In(11893) = 1         '�˂����{3��~6�܂őҋ@
1122 MRtn = fScrewTighenRoboCheck(11893)    '��~��Ԃ���M����
1123 If MRtn = 0 Then Mov PInitialPosition1  '��~�ʒu�Ɉړ�
1124 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1125 '
1126 If M_In(11369) = 0 Then M_Out(12912) = 1    '�摜���薢�g�p�������Ńt���O����6/23����
1127 If M_In(11369) = 0 Then GoTo *SkipCheck1    '�摜���薢�g�p���W�����v
1128 *RE_FLG_SET_1
1129 'Wait M_In(11920) = 0                'BaseUnit5���u����t���O�m�F(�ǉ�2/27����)
1130 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5���u����t���O�m�F(�^�C���A�E�g������ǉ�22/09/29����)
1131 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1132 If MRtn = 2 Then GoTo *RE_FLG_SET_1
1133 M_Out(12912) = 1                    '���u����t���O����
1134 'Wait M_In(11920) = 0              'Ver 0.4 �ǉ�
1135 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5���u����t���O�m�F(�^�C���A�E�g������ǉ�22/09/29����)
1136 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1137 If MRtn = 2 Then GoTo *RE_FLG_SET_1
1138 Dly 0.3
1139 'If M_In(11920) = 1 Then M_Out(12912) = 0     'Ver 0.4 �R�����g�A�E�g�@�H��6�D��
1140 If M_In(11920) = 1 Then GoTo *RE_FLG_SET_1   '
1141 '
1142 'Wait M_In(11920) = 0              'BaseUnit5���u����t���O�m�F(�ǉ���������10/1����)
1143 ''
1144 'M_Out(12912) = 1                  '���u����t���O����(�Փ˖h�~)
1145 '
1146 'Mov PBracketFCheck_2         '�o�H
1147 'Mov PBracketFCheck           '�����ʒu
1148 *RE_F_CHECK
1149 'If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck2
1150 PInspPosition(1) = PBracketFCheck1
1151 MInspGroup%(1) = 4
1152 PInspPosition(2) = PBracketFCheck2
1153 MInspGroup%(2) = 5
1154 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1155 If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck1
1156 If MRtn = 0 Then
1157     MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1158 EndIf
1159 If MRtn = 1 Then GoTo *CompFCheck
1160 fErrorProcess(11,43,46,0)
1161 If M_20# = MNext% Then M_20# = MClear%
1162 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1163     Mov PBracketFCheck
1164     Mov PInitialPosition
1165     M_Out(12912) = 0
1166 EndIf
1167 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1168 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1169 If M_20# = MContinue% Then GoTo *RE_F_CHECK
1170 *CompFCheck
1171 *SkipCheck1
1172 '
1173 '
1174 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~6�`��~7�܂�)
1175 If M_In(11369) = 1 Then Mov PBracketRCheck1
1176 'Wait M_In(11894) = 1         '�˂����{3��~7�܂őҋ@
1177 MRtn = fScrewTighenRoboCheck(11894)    '��~��Ԃ���M����
1178 If MRtn = 0 Then
1179     Mov PBracketFCheck
1180     Mov PInitialPosition
1181     M_Out(12912) = 0
1182 EndIf
1183 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1184 '
1185 If M_In(11369) = 0 Then GoTo *SkipCheck2    '�摜���薢�g�p���W�����v
1186 *RE_R_CHECK
1187 PInspPosition(1) = PBracketRCheck1
1188 MInspGroup%(1) = 2
1189 PInspPosition(2) = PBracketRCheck2
1190 MInspGroup%(2) = 3
1191 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1192 If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck2
1193 If MRtn = 0 Then
1194     MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1195 EndIf
1196 If MRtn = 1 Then GoTo *CompRCheck
1197 fErrorProcess(11,43,46,0)
1198 If M_20# = MNext% Then M_20# = MClear%
1199 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1200     Mov PBracketFCheck
1201     Mov PInitialPosition
1202     M_Out(12912) = 0
1203 EndIf
1204 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1205 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1206 If M_20# = MContinue% Then GoTo *RE_R_CHECK
1207 *CompRCheck
1208 *SkipCheck2
1209 '
1210 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~7�`��~8�܂�)�����ʒu�ύX1/20����
1211 '
1212 If M_In(11369) = 1 Then Mov PBracketFCheck
1213 '
1214 '
1215 '�˂����{3��DVDassy�����
1216 'M_Out(12866) = 1 Dly 0.5                   '�˂����{3�ɍĊJ�v��(��~7�`��~8�܂�)�����ʒu�ύX1/20����
1217 Mov PMechaOnRoboGet_3                       '�������킹
1218 'M_Out(12912) = 0                            '���u����t���O���(6/23�b��R�����g�A�E�g(����))
1219 '
1220 Mov PMechaOnRoboGet_2                       '���_(�����ʒu�ύX1/20����)
1221 '
1222 'Wait M_In(11895) = 1                       '�˂����{3��~8�܂őҋ@
1223 MRtn = fScrewTighenRoboCheck(11895)         '��~��Ԃ���M����
1224 If MRtn = 0 Then Mov PInitialPosition1      '��~�ʒu�Ɉړ�
1225 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1226 '
1227 *RE_FLG_SET_2
1228     fnAutoScreenComment(533)    '��ԕ\��[�H���T�̃��{����I���҂�] 2022/04/26 �n��
1229 'Wait M_In(11920) = 0                'BaseUnit5���u����t���O�m�F(�ǉ�2/27����)
1230 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5���u����t���O�m�F(�^�C���A�E�g������ǉ�22/09/29����)
1231 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1232 If MRtn = 2 Then GoTo *RE_FLG_SET_2'
1233 M_Out(12912) = 1                            '���u����t���O����(�Փ˖h�~)
1234 Dly 0.3
1235 'Wait M_In(11920) = 0                        'Ver 0.4 �ǉ��@�H��6�D��
1236 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5���u����t���O�m�F(�^�C���A�E�g������ǉ�22/09/29����)
1237 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1238 If MRtn = 2 Then GoTo *RE_FLG_SET_2
1239 'If M_In(11920) = 1 Then M_Out(12912) = 0   'Ver 0.4 �R�����g�A�E�g
1240 If M_In(11920) = 1 Then GoTo *RE_FLG_SET_2
1241 '
1242 'Mov PMechaOnRoboGet_2                      '�����ʒu�ύX(1/20����)
1243 Mov PMechaOnRoboGet_1                       '�˂����{���
1244 Ovrd 25
1245 '
1246 *RE_ROBO_GET_1
1247 '
1248     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1249 Mvs PMechaOnRoboGet          'DVD���J�󂯎��ʒu
1250 Dly 0.1
1251 M_Out(12257) = 0             'DVD�`���b�N�JOFF
1252 M_Out(12256) = 1             'DVD�`���b�N��ON
1253 '
1254 'Wait M_In(11266) = 1         'DVD���J�`���b�N���o
1255 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1256 If MRtn = 1 Then GoTo *CompRoboGet1
1257 fErrorProcess(11,269,284,0)
1258 If M_20# = MNext% Then M_20# = MClear%
1259 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1260 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1261 If M_20# = MContinue% Then GoTo *RE_ROBO_GET_1
1262 *CompRoboGet1
1263 '
1264 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~8�`��~9�܂�)
1265 '
1266 'Wait M_In(11876) = 1         '�˂����{3�˂����ߊ�������M
1267 '
1268 'Wait M_In(11896) = 1         '�˂����{3��~9�܂őҋ@
1269 MRtn = fScrewTighenRoboCheck(11876)    '��~��Ԃ���M����
1270 'If MRtn = 0 Then
1271 '    M_Out(12256) = 0             'DVD���J�`���b�N��OFF
1272 '    M_Out(12257) = 1             'DVD���J�`���b�N�JON
1273 '    Mvs PMechaOnRoboGet_1
1274 '    Mov PMechaOnRoboGet_2
1275 '    Mov PInitialPosition
1276 'EndIf
1277 If MRtn = 0 Then GoTo *ASSY_ERROR_END   '���̏�Œ�~
1278 '
1279 'Wait M_In(11264) = 1         'DVD���J���o
1280 *RE_ROBO_GET_2
1281 MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   'DVD���J���o
1282 If MRtn = 1 Then GoTo *CompRoboGet2
1283 fErrorProcess(11,273,284,0)
1284 If M_20# = MNext% Then M_20# = MClear%
1285 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1286 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1287 If M_20# = MContinue% Then GoTo *RE_ROBO_GET_2
1288 *CompRoboGet2
1289 '
1290 Mvs PMechaOnRoboGet_1        '�˂����{���
1291 Ovrd 100
1292 Mov PMechaOnRoboGet_2        '�˂����{���_
1293 'M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~9�`�˂����ߊ����܂�)
1294 M_Out(12912) = 0                  '���u����t���O���(�ǉ�10/1����)
1295 '
1296 Mov PTicketRead_2
1297 M_Out(12868) = 1 Dly 0.5     '�˂����{3�˂����ߊ����𑗐M(�����ʒu�ύX3/26����)
1298 '
1299     If M_22# = MAssyOK% Then GoTo *CompRead
1300 *IRREGULAR      '�J�n��Assy������DVD���J�c���W�����v��
1301     Mov PTicketRead_1
1302 '
1303 'DVDassy���p���b�g�֒u��
1304 '    Wait M_In(11218) = 1         '�p���b�g���㏸���Ă��邱�Ƃ��m�F
1305     If M_22# <> MClear% And M_22# <> MIrregular% Then GoTo *RE_PIAS_CHECK
1306     fnAutoScreenComment(95)    '��ԕ\��[�p���b�g�����ҋ@��] 2022/04/26 �n��
1307     Wait M_In(11354) = 1         ' �g���J�n�M�����o�Ă��邱�Ƃ��m�F
1308     M_Out(12346) = 1 Dly 0.5         ' �g���J�n����M
1309     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1310 *RE_PIAS_CHECK
1311     M_20# = MClear%                 '������
1312     MRtn = 1
1313     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
1314         If M_22# = MClear% Or M_22# = MContinue% Or M_22# = MIrregular% Then'PIAS�`�F�b�N�ɂĖ����������g���C���ɓ���
1315                 Mvs PTicketRead
1316                 MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
1317 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1318 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1319         EndIf
1320     EndIf
1321 '
1322     If M_22# = MPass% Then M_20# = MPass%
1323     M_22# = MClear%
1324     If MRtn = 1 And M_20# = MClear% Then GoTo *CompRead
1325 '    fErrorProcess(11,17,0,0)
1326 '    Dly 10                                      '�f�o�b�O�p
1327 '    If M_In(11359) = 1 Then M_22# = MIrregular%'�f�o�b�O�p
1328 '    If M_22# = MIrregular% Then *ASSY_ERROR_END  '�f�o�b�O�p
1329 '    If M_20# = MPass% Then M_20# = MClear%      '�f�o�b�O�p
1330     If M_20# = MPass% Then
1331         M_22# = MIrregular%
1332         M_20# = MAssyOK%
1333         Dly 0.1
1334         Mvs PTicketRead_1                         '22/04/07 �ǉ� �n��
1335     EndIf
1336     If M_20# = MAssyOK% Then GoTo *AssyEnd
1337     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1338     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1339     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1340     If M_20# = MContinue% Then GoTo *RE_PIAS_CHECK
1341 *CompRead
1342 '
1343 'Mov PTicketRead_1
1344 Accel 25 , 25                '�f�o�b�O��(3/28����)
1345 Mov PMechaOnPltSet_2         '�p���b�g���_
1346 Accel 100 , 100              '�f�o�b�O��(3/28����)
1347 Mov PMechaOnPltSet_1         '�p���b�g���
1348 '
1349 '�u���O��DVD�������Ă��邩�H�m�F    2022/04/12 �n��
1350 'DVD�������Ă����炸�A�`���b�N�̏ꍇ�A�[�̐M����ON���Ȃ�
1351 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1352 If MRtn = 1 Then GoTo *DVDCheckEnd         '�`���b�N�̕[��ON�̏ꍇ
1353 fErrorProcess(11,269,284,0)
1354 If M_20# = MNext% Then M_20# = MClear%
1355 If M_20# = MClear% Then GoTo *CompPltSet
1356 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1357 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1358 If M_20# = MContinue% Then GoTo *CompRead
1359 *DVDCheckEnd
1360 '
1361 Dly 0.1
1362 Ovrd 10
1363 Mvs PMechaOnPltSet           'DVD���J�u���ꏊ
1364 Dly 0.1
1365 '
1366 *RE_PLT_SET
1367 '
1368 M_Out(12256) = 0             'DVD���J�`���b�N��OFF
1369 M_Out(12257) = 1             'DVD���J�`���b�N�JON
1370 '
1371 'Wait M_In(11265) = 1         'DVD���J�`���b�N�J���o
1372 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1373 If MRtn = 1 Then GoTo *CompPltSet
1374 fErrorProcess(11,270,284,0)
1375 If M_20# = MNext% Then M_20# = MClear%
1376 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1377 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1378 If M_20# = MContinue% Then GoTo *RE_PLT_SET
1379 *CompPltSet
1380 M_22# = MClear%
1381 '
1382 Ovrd 100
1383 Mvs PMechaOnPltSet_1         '�p���b�g���
1384 'Ovrd 100
1385     MRtn = FnCtlValue2(2)          '�g���n�j�{�P  2022/04/28 �n��
1386 Mov PMechaOnJigGet_3
1387     MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1388 'Mov PInitialPosition   '�R�����g�A�E�g(1/20����)
1389 '
1390 'Wait M_In(11876) = 1         '�˂����{3�˂����ߊ�������M
1391 'MRtn = frInCheck(11876,1,MSETTIMEOUT05&)   '�˂����{3�˂����ߊ�������M�E
1392 'If MRtn = 0 Then
1393 '    fErrorProcess()         '�G���[����
1394 'EndIf
1395 'M_Out(12868) = 1 Dly 0.5     '�˂����{3�˂����ߊ����𑗐M(�����ʒu�ύX3/26����)
1396 '�`�P�b�gID��������
1397 'If M_20# <> MPass% Then M_20# = MAssyOK%
1398 M_20# = MAssyOK%
1399 '
1400 *ASSY_ERROR_END
1401 *AssyEnd
1402 *fnAssyStart_FEndPosi
1403 FEnd
1404 '
1405 '��fnPiasCheck
1406 ''' <summary>
1407 ''' PIAS�`�P�b�g�Ǎ���
1408 ''' </summary>
1409 ''' <returns>   0 : NG
1410 '''             1 : OK(�Ǎ��݊���)
1411 ''' </returns>
1412 ''' <remarks>
1413 ''' Date   : 2021/07/07 : M.Hayakawa
1414 ''' </remarks>'
1415 Function M% fnPiasCheck
1416     fnPiasCheck = 0
1417     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1418     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1419 '
1420 *RETRY_PIAS
1421     M_20# = MClear%
1422     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1423     '
1424     '�yID�`�P�b�g�ǂݍ��݁z
1425     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1426     MInspGroup%(1) = 1              '����G�ԍ�
1427     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1428 '
1429     '�G���[�̏ꍇ
1430     If MRtn <> 1 Then
1431         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1432         If MRtn <> 1 Then
1433             'D720 -> D1300 �R�s�[�v��
1434             M_Out(12565) = 1
1435             Dly 0.5
1436             M_Out(12565) = 0
1437             '�G���[�����L�q
1438             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1439             'GOT KEY���͑҂�
1440             MKeyNumber = fnKEY_WAIT()
1441         '
1442             Select MKeyNumber
1443                 Case MNext%         '���ւ�I�������ꍇ
1444                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1445                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1446 '                    GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��(�֐��O���ŕ��򂷂�悤�ύX3/26����)
1447                     Break
1448                 Case MAbout%        '��~��I�������ꍇ
1449                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1450                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1451 '                    GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��(�֐��O���ŕ��򂷂�悤�ύX3/26����)
1452                     Break
1453                 Case MNgProcess%    'NG��I�������ꍇ
1454                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1455                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1456 '                    GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��(�֐��O���ŕ��򂷂�悤�ύX3/26����)
1457                     Break
1458                 Case MContinue%     '�p����I�������ꍇ
1459                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1460                     M_20# = MContinue%
1461 '                    GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C(�֐��O���ŕ��򂷂�悤�ύX3/26����)
1462                     Break
1463             End Select
1464         EndIf
1465     EndIf
1466     If M_20# <> MClear% Then GoTo *fnPiasCheck_End
1467 '
1468 '----------D720 -> D1300 �R�s�[�v��----------
1469     M_Out(12565) = 1
1470     Dly 0.5
1471     M_Out(12565) = 0
1472 '----------�ʐM�m�F������----------
1473     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1474     MRtn = 0                ' ������
1475     M_20# = MClear%         ' ������
1476     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1477     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j(�֐��O���ŕ��򂷂�悤�ɕύX3/26����)
1478     If MRtn <> 1 Then GoTo *fnPiasCheck_End
1479 '        If M_20# = MContinue% Then
1480 '            GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1481 '        Else
1482 '            GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1483 '        EndIf
1484 '    EndIf
1485 '----------�H�������m�F----------
1486     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1487     MRtn = 0                ' ������
1488     M_20# = MClear%         ' ������
1489     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1490     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j(�֐��O���ŕ��򂷂�悤�ɕύX3/26����)
1491     If MRtn <> 1 Then GoTo *fnPiasCheck_End
1492 '        If M_20# = MContinue% Then
1493 '            GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1494 '        Else
1495 '            GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1496 '        EndIf
1497 '    EndIf
1498     '
1499     fnPiasCheck = 1
1500     *fnPiasCheck_End
1501 FEnd
1502 '
1503 '��fnPCComuCheck
1504 ''' <summary>
1505 ''' PC-PLC�ʐM�`�F�b�N
1506 ''' </summary>
1507 ''' <returns>   0 : NG
1508 '''             1 : OK(�Ǎ��݊���)
1509 ''' </returns>
1510 ''' <remarks>
1511 ''' Date   : 2021/07/07 : M.Hayakawa
1512 ''' </remarks>'
1513 Function M% fnPCComuCheck
1514     fnPCComuCheck = 0
1515     MJudge% = 0                                  '������
1516     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1517     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1518     '
1519     For MStaNo = 0 To 5
1520         '
1521         If M_In(MIN_PIAS_ComOK%) = 1 Then
1522             'PC�ʐMOK(M400)
1523             MJudge% = MOK%
1524             MStaNo = 5
1525             Break
1526         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1527             'toRBT_�ʐM�m�Ftime out
1528             MJudge% = MNG%
1529             MCommentD1001 = 15
1530             MCommentD1002 = 21
1531             MStaNo = 5
1532             Break
1533         Else
1534             'toRBT_�ʐM�m�Ftime out
1535             MJudge% = MNG%
1536             MCommentD1001 = 14
1537             MCommentD1002 = 21
1538             Break
1539         EndIf
1540     Next MStaNo
1541     '
1542     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1543     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1544     '
1545     '�G���[���
1546     If MJudge% <> MOK% Then
1547         M_20# = MClear%     '������
1548         '�G���[�����L�q
1549         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1550         'GOT KEY���͑҂�
1551         MKeyNumber = fnKEY_WAIT()
1552         '
1553         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1554             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1555             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1556             Break
1557         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1558             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1559             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1560             Break
1561         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1562             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1563             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1564             Break
1565         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1566             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1567             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1568             Break
1569         EndIf
1570     Else
1571         'OK�̏ꍇ
1572         fnPCComuCheck = 1
1573     EndIf
1574 FEnd
1575 '
1576 '��fnProcessCheck
1577 ''' <summary>
1578 ''' �H�������m�F
1579 ''' </summary>
1580 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1581 '''             -1�F�O�H������NG  -2�F���H����������
1582 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1583 '''             -5�F���������G���[
1584 ''' </returns>
1585 ''' <remarks>
1586 ''' Date   : 2021/07/07 : M.Hayakawa
1587 ''' </remarks>'
1588 Function M% fnProcessCheck
1589     fnProcessCheck = 0
1590     MJudge% = MNG%      '��UNG���������Ƃ���
1591 '----------�H�������m�F----------
1592     MCommentD1001 = 0   '�R�����g������
1593     For MStaNo = 0 To 5
1594         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1595         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1596         '
1597         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1598             MJudge% = MOK%
1599             fnAutoScreenComment(85)     ' AUTO���
1600             MStaNo = 5
1601             Break
1602         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1603             MFlgLoop% = 0
1604             MJudge% = MNG%
1605             MCommentD1001 = 27
1606             MCommentD1002 = 22
1607             fnAutoScreenComment(94)     ' AUTO���
1608             fnProcessCheck = -2         ' NG��-2��Ԃ�
1609             MStaNo = 5
1610             Break
1611         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1612            MJudge% = MNG%
1613             MCommentD1001 = 31
1614             MCommentD1002 = 22
1615             fnAutoScreenComment(83)     ' AUTO���
1616             fnProcessCheck = -3         ' NG��-3��Ԃ�
1617             MStaNo = 5
1618             Break
1619         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1620             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1621             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1622             MJudge% = MNG%
1623             MCommentD1001 = 32
1624             MCommentD1002 = 22
1625             fnAutoScreenComment(84)     ' AUTO���
1626             fnProcessCheck = -1         ' NG��-1��Ԃ�
1627             Dly 1.0
1628             '�H�������m�FOFF
1629             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1630             Dly 1.0
1631            'MStaNo = 5
1632             Break
1633         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1634             MFlgLoop% = 0
1635             MJudge% = MNG%
1636             MCommentD1001 = 29
1637             MCommentD1002 = 22
1638             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1639             fnProcessCheck = -5         ' NG��-5��Ԃ�
1640             MStaNo = 5
1641             Break
1642         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1643             MJudge% = MNG%
1644             If MCommentD1001 = 32 Then
1645                 '�������Ȃ�
1646             Else
1647                 MCommentD1001 = 26
1648             EndIf
1649             MCommentD1002 = 22
1650             fnProcessCheck = -4         ' NG��-4��Ԃ�
1651             MStaNo = 5
1652             Break
1653         Else
1654             MJudge% = MNG%
1655             MCommentD1001 = 28
1656             MCommentD1002 = 22
1657         EndIf
1658     Next MStaNo
1659     '�H�������m�FOFF
1660     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1661     '�ʉߗ���NG �H�������̏ꍇ
1662     If MJudge% = MPass% Then
1663         M_20# = MPass%
1664     EndIf
1665     '
1666     '�G���[���
1667     If MJudge% <> MOK% Then
1668         M_20# = MClear%     '������
1669         '�G���[�����L�q
1670         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1671         'GOT KEY���͑҂�
1672         MKeyNumber = fnKEY_WAIT()
1673         '
1674         Select MKeyNumber
1675             Case MAbout%        '��~��I�������ꍇ
1676                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1677                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1678                 Break
1679             Case MNext%         '���ւ�I�������ꍇ
1680                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1681                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1682                 Break
1683             Case MContinue%     '�p����I�������ꍇ
1684                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1685                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1686                 Break
1687             Case MNgProcess%    'NG��I�������ꍇ
1688                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1689                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1690                 Break
1691         End Select
1692     Else
1693         fnProcessCheck = 1  ' OK��1��Ԃ�
1694     EndIf
1695 FEnd
1696 '
1697 '��fnPiasWrite
1698 ''' <summary>
1699 ''' Pias �g�����ʏ����ݗv��
1700 ''' </summary>
1701 '''<param name="MFlg%">
1702 '''                 MOK%(1) = �H��������OK��������
1703 '''                 MNG%(0) = �H��������NG��������
1704 '''</param>
1705 '''<returns></returns>
1706 ''' <remarks>
1707 ''' Date   : 2021/07/07 : M.Hayakawa
1708 ''' </remarks>'
1709 Function M% fnPiasWrite(ByVal MFlg%)
1710       fnPiasWrite = 0
1711 *RETRY_PIASWRITE
1712     '
1713     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1714    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1715     If MFlg% = MOK% Then
1716         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1717     Else
1718         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1719     EndIf
1720     Dly 0.1                  '�O�̂���
1721     '
1722     'Pias�֏����݊J�n M305 -> ON
1723     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1724     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1725     '
1726     MJudge% = MNG%
1727     '
1728     For MStaNo = 0 To 5
1729         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1730             MJudge% = MOK%
1731             'MRet = fnAutoScreenComment(85)  'AUTO���
1732             MStaNo = 5
1733             Break
1734         '
1735         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1736             MJudge% = MNG%
1737             'MRet = fnAutoScreenComment(85)  'AUTO���
1738            MCommentD1001 = 34
1739            MCommentD1002 = 25
1740             MStaNo = 5
1741             Break
1742         '
1743         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1744             MJudge% = MNG%
1745             'MRet = fnAutoScreenComment(85)  'AUTO���
1746            MCommentD1001 = 35
1747            MCommentD1002 = 25
1748             MStaNo = 5
1749             Break
1750         '
1751         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1752             MJudge% = MNG%
1753             'MRet = fnAutoScreenComment(85)  'AUTO���
1754            MCommentD1001 = 36
1755            MCommentD1002 = 25
1756             MStaNo = 5
1757             Break
1758         '
1759         Else
1760             MJudge% = MNG%
1761            MCommentD1001 = 42
1762            MCommentD1002 = 25
1763         '
1764         EndIf
1765         '
1766     Next MStaNo
1767     '
1768     'Pias�֏����݊J�n M305 -> OfF
1769     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1770     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1771     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1772     '
1773     '
1774     '�ʉߗ���NG �H�������̏ꍇ
1775     If MJudge% = MPass% Then
1776         M_20# = MPass%
1777     EndIf
1778     '
1779    M_20# = MClear%     '������
1780     '
1781     '�G���[���
1782     If MJudge% < MOK% Then
1783     '
1784 '�c���Ă���������ł͎g�p���Ȃ����x��
1785 *RETRY_ERR_WRITE
1786         M_20# = MClear%     '������
1787         '�G���[�����L�q
1788         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1789         'GOT KEY���͑҂�
1790         MKeyNumber = fnKEY_WAIT()
1791         '
1792         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1793             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1794            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1795             Break
1796         '
1797         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1798             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1799             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1800         '
1801         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1802             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1803             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1804         '
1805         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1806             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1807            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1808             Break
1809         '
1810         EndIf
1811         '
1812         If M_20# = MClear% Then *RETRY_ERR_WRITE
1813         '
1814     EndIf
1815     '
1816     If M_20# = MContinue% Then *RETRY_PIASWRITE
1817     '
1818     fnPiasWrite = 1
1819     '
1820 FEnd
1821 '
1822 '��fnPCBNumberCheck
1823 ''' <summary>
1824 ''' Pias ��ԍ��ƍ��v��
1825 ''' </summary>
1826 '''<param name="%"></param>
1827 '''<param name="%"></param>
1828 '''<returns></returns>
1829 ''' <remarks>
1830 ''' Date   : 2021/07/07 : M.Hayakawa
1831 ''' </remarks>'
1832 Function M% fnPCBNumberCheck
1833       fnPCBNumberCheck = 0
1834     '
1835 *RETRY_PCBCHECK
1836     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1837     'Pias�֊�ƍ��J�n M310 -> ON
1838     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1839     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1840     '
1841     MJudge% = MNG%
1842     '
1843     For MStaNo = 0 To 5
1844         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1845             MJudge% = MOK%
1846             fnAutoScreenComment(96)  'AUTO���
1847             MStaNo = 5
1848             Break
1849         '
1850         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1851             MJudge% = MNG%
1852             fnAutoScreenComment(97)  'AUTO���
1853             MCommentD1001 = 37
1854             MCommentD1002 = 25
1855             MStaNo = 5
1856             Break
1857         '
1858         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1859             MJudge% = MNG%
1860             fnAutoScreenComment(98)  'AUTO���
1861             MCommentD1001 = 38
1862             MCommentD1002 = 25
1863             MStaNo = 5
1864             Break
1865         '
1866         ElseIf M_In(11580) = 1 Then                         'time out
1867             MJudge% = MNG%
1868             fnAutoScreenComment(99)  'AUTO���
1869             MCommentD1001 = 39
1870             MCommentD1002 = 25
1871             MStaNo = 5
1872             Break
1873         '
1874         Else
1875             MJudge% = MNG%
1876            MCommentD1001 = 41
1877            MCommentD1002 = 25
1878         '
1879         EndIf
1880         '
1881     Next MStaNo
1882     '
1883     'Pias�֊�ƍ��J�n M310 -> OfF
1884     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1885     '
1886     '
1887     '�ʉߗ���NG �H�������̏ꍇ
1888     If MJudge% = MPass% Then
1889         M_20# = MPass%
1890     EndIf
1891     '
1892    M_20# = MClear%     '������
1893     '
1894     '�G���[���
1895     If MJudge% < MOK% Then
1896     '
1897 '�c���Ă���������ł͎g�p���Ȃ����x��
1898 *RETRY_ERR_PCBNUMBER
1899         M_20# = MClear%     '������
1900         '�G���[�����L�q
1901         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1902         'GOT KEY���͑҂�
1903         MKeyNumber = fnKEY_WAIT()
1904         '
1905         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1906             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1907             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1908             Break
1909         '
1910         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1911             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1912             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1913         '
1914         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1915             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1916             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1917         '
1918         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1919             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1920             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1921             Break
1922         '
1923         EndIf
1924         '
1925         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1926         '
1927     EndIf
1928     '
1929     If M_20# = MContinue% Then *RETRY_PCBCHECK
1930 FEnd
1931 '
1932 '��ScrewTight_S2
1933 ''' <summary>
1934 ''' �˂����߂��s��
1935 ''' </summary>
1936 '''<param name="PScrewPos()">
1937 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1938 '''             PScrewPos(2)    �F�˂����߉��_
1939 '''             PScrewPos(10)   �F�˂����ߏI������
1940 '''</param>
1941 '''<returns>����
1942 '''         0=�ُ�I���A1=����I��
1943 '''</returns>
1944 ''' <remarks>
1945 ''' Date   : 2021/07/07 : M.Hayakawa
1946 ''' </remarks>'
1947 Function M% ScrewTight_S2(ByVal PScrewPosition())   '�l�W���ߌʐݒ�
1948     ScrewTight_S2 = 0
1949     MOKNGFlg = 0
1950     Ovrd 100
1951     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1952     ' �b��
1953     Ovrd 5
1954     Mvs PScrewPosition(10),-10    ' �p���b�g��˂�����S�@�̏��ֈړ�
1955 '    Ovrd MOvrdA
1956     '�b��}�X�N
1957 '    M_Out(Y62_Driver)=1     ' �o���N�Z�b�e�B���O�@C1
1958 '    Dly 0.1
1959 '    M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
1960 '    'Spd 8.3 '�O�����C�h100  �������C�h60   '���C�h100-40�@100%�FSpd�@15�@'�˂����ߑ��x�ݒ�
1961 '    Spd MSpdA               '�l�W���ߎ�Spd�ʐݒ�
1962     ' �b��ړ��̂�
1963     Mvs PScrewPosition(10)
1964 '    '
1965 '    Dly 0.1
1966 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1967 '    Wait M_In(11584)=1          '����/�G���[���o
1968 '    Dly 0.1
1969 '    Spd M_NSpd
1970 '    '
1971 '    If M_In(X28_Driver)=1 Then  '�˂��g�[�^���G���[���o��
1972 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1973 '        Dly 0.1
1974 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1975 '        Dly 0.1
1976 '        M_Out(Y63_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1977 '        Dly 0.1
1978 '        M_Out(Y65_Driver)=0     '�v���O���������@F1
1979 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1980 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
1981 '        MOKNGFlg = -1
1982 '        ScrewTight_S2 = 0
1983 '    Else
1984 '        Wait M_In(X29_Driver)=1 ' ���튮����
1985 '        Dly 0.1
1986 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1987 '        Dly 0.1
1988 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O����
1989 '        Dly 0.1
1990 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
1991 '        Dly 0.1
1992 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1993 '        ScrewTight_S2 = 1
1994 '    EndIf
1995 ' �b��
1996     Ovrd 10
1997     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1998     Ovrd 100
1999 FEnd
2000 '
2001 '��ScrewGet_S3
2002 ''' <summary>
2003 ''' �˂������@����˂��𓾂�
2004 ''' </summary>
2005 '''<param name="%"></param>
2006 '''         PScrewPos(1)    �F�˂�������̂˂����
2007 '''         PScrewPos(2)    �F�˂���������_
2008 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2009 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
2010 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
2011 '''<returns>����
2012 '''         0=�ُ�I���A1=����I���A-1=M�l�W�Z���T�[NG�A-2=M�l�W�Z���T�[ON�A-3=�z���G���[
2013 '''</returns>
2014 ''' <remarks>
2015 ''' Date   : 2021/07/07 : M.Hayakawa
2016 ''' </remarks>'
2017 Function M% ScrewGet_S3(ByVal PScrewPosition())
2018     ScrewGet_S3 = 0
2019     MMScrewJudge% = 0
2020     '�˂������평������G���[�`�F�b�N
2021 ' ���b��폜
2022 '    Wait M_In(X34_ScrewReady1)=1 '�˂�������S��Ready�ɂȂ�܂ő҂@���@���b���҂���Ready�ɂȂ�Ȃ���Δ�����v���O�������K�v�H
2023 '    Ovrd 100
2024 '    If M_In(X33_SS2)=0 Then  'M�˂����o�Z���T��OFF�i�̏�j���Ă����ꍇ
2025 '        Ovrd 30
2026 '        Mvs,-80             '���̏ꏊ����80mm���ֈړ�
2027 '        Mov PInitPos19049   '19049�����ʒu�ֈړ�
2028 '        M_Out(Y6A_VV1)=0    '�˂��z�� Off
2029 '        'NG�Ƃ��Ă����̊֐����甲����
2030 '        ScrewGet_S3 = -1
2031 '        MMScrewJudge% = 1
2032 '        MCommentD1001 = 61
2033 '    EndIf
2034 '    If ScrewGet_S3 = 0 Then
2035 '        'S�^�C�g�p�˂������@��M�˂����������Ă��Ȃ����Ď�
2036 '        MMScrewJudge% = 0 'MMScrewJudge������������
2037 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT05&)
2038 '        If MRtn = 0 Then
2039 '            Ovrd 30
2040 '            Mvs,-80            '���̏ꏊ����50mm���ֈړ�
2041 '            Mov PInitPos19049  '19049�����ʒu�ֈړ�
2042 '            MMScrewJudge% = 2
2043 '            MRtn = All_CLamp_Release()'�S�ẴN�����v�����֕���
2044 '            MCnt% = 2   '2��ݒ�
2045 '            MCommentD1001 = 62
2046 '        EndIf
2047 '        If MMScrewJudge% = 2 Then
2048 '            ScrewGet_S3 = -2
2049 '        EndIf
2050 '    EndIf
2051 '    'M�l�W���肪ON�̏ꍇ NG�Ƃ��Ċ֐��𔲂���
2052 '    If MMScrewJudge% = 2 Then
2053 '        ScrewGet_S3 = -2
2054 '    EndIf
2055     'S�l�W�p�˂����Y��M�l�W�����m�F�p�����܂�
2056     Ovrd 100
2057     Spd M_NSpd
2058     If MMScrewJudge% = 0 Then
2059         ScrewGet_S3 = 0
2060         M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
2061         MScrewCnt% = 0
2062         MFinCnt% = 2
2063 '        For MCnt% = 0 To MFinCnt%
2064             Mov PScrewPosition(2)        ' �˂������@���_
2065             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
2066             Ovrd 80
2067             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2068             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
2069             Mvs PScrewPosition(10), 1.2
2070             M_Out(Y6A_VV1)=1        ' �˂��z���@ON
2071             '�r�b�g��]
2072             M_Out(Y60_Driver)=1
2073             Dly 0.2
2074             '
2075             Ovrd 100
2076             JOvrd M_NJovrd
2077             Spd M_NSpd
2078             '�l�W�z���m�F�ʒu�ړ�
2079             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
2080             Mvs PScrewPosition(10), -15  ' �l�W�z���m�F�ʒu
2081             '�r�b�g��]��~
2082             'M_Out(Y60_Driver)=0
2083             '
2084             '1�b�ԃl�W�z���m�F
2085 ' �ȉ��b��폜
2086 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2087 '            'MRtn = 0'�����G���[
2088 '            '�z���G���[�̏ꍇ
2089 '            '�l�W���˂����Y�ɖ߂�
2090 '            If MRtn = 0 Then
2091 '                Ovrd 30
2092 '                '�r�b�g��]��~
2093 '                M_Out(Y60_Driver)=0
2094 '                '�l�W�����@���
2095 '                Mvs PScrewPos(1)
2096 '                '�X�ɏ��
2097 '                Mov PScrewPos(1), -75
2098 '                '�l�W�̂Ĉʒu
2099 '                Mov PScrewFeedS021
2100 '                '�z��OFF
2101 '                M_Out(Y6A_VV1)=0 '�˂��z���@OFF
2102 '                Dly 0.2
2103 '                '�j��ON
2104 '                M_Out(Y6B_VB1)=1 '�^��j��ON
2105 '                '�r�b�g��]
2106 '                M_Out(Y61_Driver)=1
2107 '                Dly 0.5
2108 '                '
2109 '                Ovrd 100
2110 '                JOvrd M_NJovrd
2111 '                Spd M_NSpd
2112 '                '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
2113 '                Mov PScrewFeedS021, 10
2114 '                Mov PScrewFeedS021
2115 '                Dly 0.1
2116 '                Mov PScrewFeedS021, 10
2117 '                Mov PScrewFeedS021
2118 '                '
2119 '                '�l�W�����҂�
2120 '                '�r�b�g��]��~
2121 '                M_Out(Y61_Driver)=0
2122 '                Dly 0.1
2123 '                '�j��OFF
2124 '                M_Out(Y6B_VB1)=0 '�^��j��OFF
2125 '                '
2126 '                '
2127 '                '�˂��������Ƃ��āA�ړ��X�ɏ��
2128 '                Mov PScrewPos(1), -75
2129 '                Ovrd 100
2130 '                Spd M_NSpd
2131 '                '�l�W�����@���
2132 '                Mvs PScrewPos(1)
2133 '                '
2134 '                ScrewGet_S3 = -3
2135 '                Break
2136 '                '
2137 '            Else
2138 '                MCnt% = MFinCnt%
2139 '                ScrewGet_S3 = 0
2140 '            EndIf
2141 '        Next  MCnt%
2142         '
2143         Ovrd 100
2144         Spd M_NSpd
2145         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2146         M_Out(Y60_Driver)=0     ' �r�b�g��]��~
2147         M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
2148         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2149         '������x�z���m�F
2150 ' �ȉ��b��폜
2151 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2152 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
2153 '            MCommentD1001 = 94
2154 '            MCommentD1002 = 95
2155 '            ScrewGet_S3 = -3
2156 '        EndIf
2157 '        If MRtn = 1 Then      '�z��OK�̏ꍇ
2158 '            ScrewGet_S3 = 1
2159 '        EndIf
2160 '        Break
2161     Else
2162         'M�l�W
2163         If MMScrewJudge% = 2 Then
2164             ScrewGet_S3 = -2
2165         EndIf
2166     EndIf
2167 FEnd
2168 '
2169 '��fnKEY_WAIT()
2170 ''' <summary>
2171 ''' GOT����̃L�[���͑҂�
2172 ''' </summary>
2173 '''<returns>1�F��~    2�F����
2174 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
2175 '''         5�FNG
2176 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
2177 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
2178 '''</returns>
2179 ''' <remarks>
2180 ''' Date   : 2021/07/07 : M.Hayakawa
2181 ''' </remarks>'
2182 Function M% fnKEY_WAIT()
2183     fnKEY_WAIT = 0
2184     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
2185     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
2186     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
2187     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2188     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
2189     Dly 0.2
2190     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
2191     MLocalLoopFlg=1
2192     While MLocalLoopFlg=1
2193         If M_In(11345) = 1 Then         '��~   M5345
2194             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
2195             fnKEY_WAIT = 1
2196             MLocalLoopFlg=-1
2197             Break
2198         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
2199             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
2200             fnKEY_WAIT = 2
2201             MLocalLoopFlg=-1
2202             Break
2203         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
2204             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
2205             fnKEY_WAIT = 3
2206             MLocalLoopFlg=-1
2207             Break
2208         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
2209             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
2210             fnKEY_WAIT = 4
2211             MLocalLoopFlg=-1
2212             Break
2213         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
2214             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
2215             fnKEY_WAIT = 5
2216             MLocalLoopFlg=-1
2217             Break
2218             '
2219         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
2220             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
2221             fnKEY_WAIT = MRobotInit1%
2222             MLocalLoopFlg=-1
2223             Break
2224             '
2225         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
2226             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
2227             fnKEY_WAIT = MRobotInit2%
2228             MLocalLoopFlg=-1
2229             Break
2230             '
2231         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
2232             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
2233             fnKEY_WAIT = MRobotInit3%
2234             MLocalLoopFlg=-1
2235             Break
2236             '
2237         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
2238             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2239             fnKEY_WAIT = MRobotInit4%
2240             MLocalLoopFlg=-1
2241             Break
2242             '
2243         Else
2244         EndIf
2245     WEnd
2246     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2247     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2248 FEnd
2249 '
2250 '�� fnAUTO_CTL
2251 ''' <summary>
2252 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2253 ''' </summary>
2254 ''' <remarks>
2255 ''' Date   : 2021/07/07 : M.Hayakawa
2256 ''' </remarks>
2257 Function M% fnAUTO_CTL
2258     fnAUTO_CTL = 0
2259     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2260     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2261     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2262     '
2263     If M_Svo=0 Then             '�T�[�{ON�m�F
2264         Servo On
2265     EndIf
2266     Wait M_Svo=1
2267 FEnd
2268 '
2269 '�� fnWindScreenOpen
2270 ''' <summary>
2271 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2272 ''' </summary>
2273 '''<param name="%"></param>
2274 '''<param name="%"></param>
2275 '''<param name="%"></param>
2276 '''<param name="%"></param>
2277 ''' <remarks>
2278 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2279 ''' MWindReSet = 0     ��ʔ�\��
2280 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2281 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2282 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2283 ''' Date   : 2021/07/07 : M.Hayakawa
2284 ''' </remarks>
2285 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2286     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2287         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2288     EndIf
2289     '
2290     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2291         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2292     EndIf
2293     '
2294     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2295        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2296     EndIf
2297     '
2298     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2299     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2300     Dly 0.5
2301     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2302 FEnd
2303 '
2304 '��FnCtlValue2
2305 ''' <summary>
2306 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2307 ''' </summary>
2308 ''' <param name="MCtlNo%"></param>
2309 ''' <remarks>
2310 ''' Date : 2022/04/28 �n��
2311 ''' </remarks>
2312 '''
2313 '''  1�F������       �{�P
2314 '''  2�F�g���n�j��   �{�P
2315 '''  3�F�g���m�f��   �{�P (���g�p)
2316 '''  4�F�z���G���[�� �{�P
2317 ''' 99�F�Ǐ��J�n�M�� OFF
2318 '''
2319 Function M% FnCtlValue2(ByVal MCtlNo%)
2320     FnCtlValue2 = 1
2321     Select MCtlNo%
2322         Case 1        '�������{�P
2323             M_Out(12569) = 0             '�����݊J�n�M��OFF
2324             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2325             MInputQty = M_In16(11600)    '��������M
2326             MInputQty = MInputQty + 1    '�������{�P
2327             M_Out16(12592) = MInputQty   '���������M
2328             M_Out(12569) = 1             '�����݊J�n�M��ON
2329             Break
2330             '
2331         Case 2        '�g���n�j���{�P
2332             M_Out(12569) = 0             '�����݊J�n�M��OFF
2333             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2334             MAssyOkQty = M_In16(11616)   '�g��OK����M
2335             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2336             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2337             M_Out(12569) = 1             '�����݊J�n�M��ON
2338             Break
2339             '
2340         Case 4        '�z���G���[���{�P
2341             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2342             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2343             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2344             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2345             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2346             M_Out(12569) = 1                       '�����݊J�n�M��ON
2347             Break
2348             '
2349         Case 99        '�Ǐ��J�n�M��OFF
2350             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2351             M_Out(12569) = 0        '�����݊J�n�M��OFF
2352             Break
2353             '
2354     End Select
2355     Exit Function
2356 FEnd
2357 '
2358 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2359 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2360 '-------------------------------------------------------------------------------
2361 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2362 '   ����
2363 '       PInspPos()      �F�����ʒu
2364 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2365 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2366 '       MInspCnt%       �F�����ʒu��
2367 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2368 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2369 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2370 '   �߂�l�F����
2371 '       0=�ُ�I���A1=����I��
2372 '
2373 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2374 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2375 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2376 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2377 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2378 '-------------------------------------------------------------------------------
2379     '----- �����ݒ� -----
2380     Cnt 0                                                           '�ړ�����������(�����l=0)
2381     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2382 '    Cnt 1,0.1,0.1
2383     '�ϐ��錾�E������
2384     Def Inte MNum                                                   '�����ԍ�(������1�`)
2385     MNum% = 1                                                       '�����ԍ������l�ݒ�
2386     Def Inte MEndFlg                                                '�����I���t���O
2387     MEndFlg% = 0
2388     '
2389     '����G�ԍ��ݒ�v���E�������s�v��off
2390     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2391     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2392     '�G���[�ԍ��N���A
2393     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2394     M_Out16(MOUT_InspErrNum) = MInspErrNum
2395     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2396     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2397     '
2398     'Insight Ready check?
2399     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2400         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2401         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2402         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2403         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2404         Exit Function
2405     EndIf
2406     '
2407     '�����ʒu���m�F
2408     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2409         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2410         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2411         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2412         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2413         Exit Function
2414     EndIf
2415     '
2416     '
2417     '
2418     '----- ���C������ -----
2419     '�ݒ肳�ꂽ�����ʒu�����̌������s
2420     While( MEndFlg% = 0 )
2421         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2422         MSetGrNumRetryExitFlg = 0
2423         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2424         While( MSetGrNumRetryExitFlg = 0 )
2425         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2426             '
2427             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2428             '
2429             '----- �����O���[�v�ԍ��ݒ� -----
2430             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2431             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2432             '
2433             '�����ʒu�ֈړ��E�ړ������҂�
2434             Mvs PInspPos( MNum% )                                       '�ړ�
2435             Dly 0.05                                                    '�ړ�������Delay
2436             '
2437             '�����O���[�v�ԍ��ݒ�I���m�F
2438             M_Timer(1) = 0
2439             MExitFlg = 0
2440             While( MExitFlg = 0 )
2441                 '����G�ݒ萳��I��?
2442                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2443                     MExitFlg = 1
2444                 '
2445                 '����G�ݒ�ُ�I��?
2446                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2447                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2448                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2449                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2450                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2451                     EndIf
2452                     MExitFlg = 1
2453                 '
2454                 'timeout�`�F�b�N
2455                 ElseIf 1000 < M_Timer(1) Then
2456                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2457                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2458                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2459                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2460                     EndIf
2461                     MExitFlg = 1
2462                 EndIf
2463             WEnd
2464             '
2465             '����G�ԍ��ݒ�v��off
2466             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2467             '
2468             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2469             'NG�Ȃ���Δ�����
2470             If MCurrentStepErr = 0 Then
2471                 MSetGrNumRetryExitFlg = 1
2472             Else
2473                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2474                 If MSetGrNumRetryCnt = 0 Then
2475                     MSetGrNumRetryExitFlg = 1
2476                 Else
2477                     'Retry�ց@���̑O��Delay
2478                     Dly 0.5
2479                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2480                 EndIf
2481             EndIf
2482             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2483             '
2484         WEnd
2485         '
2486         '
2487         '
2488         '----- �������s -----
2489         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2490             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2491                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2492                 MInspRetryExitFlg = 0
2493                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2494                 While( MInspRetryExitFlg = 0 )
2495                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2496                     '
2497                     '���������m�F
2498                     MRetryCnt = MRetryCnt - 1
2499                     M_Timer(1) = 0
2500                     MExitFlg = 0
2501                     While( MExitFlg = 0 )
2502                     '���������҂�
2503                         '����OK�I��?
2504                         If M_In( MIN_IS_InspOK% ) = 1  Then
2505                             MJudgeOKFlg = 1                         '����OK�t���OON
2506                             MExitFlg = 1
2507                         '
2508                         '����NG�I��?
2509                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2510                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2511                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2512                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2513                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2514                                 EndIf
2515                             EndIf
2516                             MExitFlg = 1
2517                         '
2518                         '�����ُ�I��(IS timeout)?
2519                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2520                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2521                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2522                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2523                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2524                                 EndIf
2525                             EndIf
2526                             MExitFlg = 1
2527                         '
2528                         'timeout�`�F�b�N
2529                         ElseIf 3000 < M_Timer(1) Then
2530                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2531                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2532                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2533                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2534                                 EndIf
2535                             EndIf
2536                             MExitFlg = 1
2537                         EndIf
2538                     WEnd
2539                     '
2540                     '�����J�n�v��off
2541                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2542                     '
2543                     'OK�Ȃ甲����
2544                     If MJudgeOKFlg = 1 Then
2545                         MInspRetryExitFlg = 1
2546                     Else
2547                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2548                         If MRetryCnt = 0 Then
2549                             MInspRetryExitFlg = 1
2550                         Else
2551                             'Retry�ց@���̑O��Delay
2552                             Dly 0.3
2553                         EndIf
2554                     EndIf
2555                     '
2556                 WEnd
2557             EndIf
2558         EndIf
2559         '
2560         '
2561         '
2562         MNum% = MNum% + 1                                           '����Step+1
2563         '�����I���m�F�@�����I���t���O�Z�b�g
2564         If (MInspCnt% < MNum% ) Then
2565             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2566         EndIf
2567         'NG���������s������
2568         If MInspErrNum <> 0 Then                                    'NG����?
2569             If MNgContinue% <> 1 Then                               'NG���s?
2570                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2571             EndIf
2572         EndIf
2573     WEnd
2574     '
2575     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2576     If 0 < MZAxis% Then
2577         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2578         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2579         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2580     EndIf
2581     '
2582     '�߂�l�ݒ�
2583     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
2584         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2585     Else
2586         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2587         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2588         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2589     EndIf
2590     Fine 0 , P
2591     '
2592 FEnd
2593 '
2594 ' ��ISInspection
2595 ''' <summary>
2596 ''' Insight�ɂ��摜�����������s
2597 ''' </summary>
2598 '''<param name="PInspPos()">�����ʒu</param>
2599 '''<param name="MInspGrNum%()">�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j</param>
2600 '''             PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2601 '''<param name="MInspCnt%">�����ʒu��</param>
2602 '''<param name="MZAxis%">�I������Z���ޔ����W�i-1:�����j</param>
2603 '''             �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2604 '''<param name="MNgContinue%">=1�Ō����G���[�ENG�������ɑSStep�̌������s��</param>
2605 '''<returns>    ���� 0=�ُ�I���A1=����I��</returns>
2606 '''         MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2607 '''         MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���"
2608 ''' <remarks>
2609 ''' Date   : 2021/07/07 : M.Hayakawa
2610 ''' </remarks>
2611 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2612 '    '�摜�g�p�m�F 0<- �摜�m�F�����̏ꍇ
2613 '    If M_In(11369) = 0 Then            'toRBT_�g�p�m�F
2614 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2615 '    EndIf
2616 ''
2617 '    Cnt 0                                                       '�ړ�����������(�����l=0)
2618 '    Fine 0.05,P                                                 '�ʒu���ߊ��������ݒu�@0.05mm
2619 '    MNum% = 1                                                   '�����ԍ������l�ݒ�
2620 '    Def Inte MEndFlg                                            '�����I���t���O
2621 '    MEndFlg% = 0
2622 '    '
2623 '    '�G���[�ԍ��N���A
2624 '    MInspErrNumSub = 0                                          '�������s�G���[�ԍ�sub
2625 '    MInspErrNum = 0                                             '�������s�G���[�ԍ�
2626 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2627 '    MInspNGStepNum = 0                                          '�������sNGStep�ԍ�
2628 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2629 '    '
2630 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready off�Ȃ�I��
2631 '        MInspErrNum = 20                                        '�������s�G���[�ԍ� 20 Insight offline
2632 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2633 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2634 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2635 ''
2636 '    EndIf
2637 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2638 '    '
2639 '    '�����ʒu���m�F
2640 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2641 '        MInspErrNum = 21                                        '�����f�[�^�Ȃ� 21�@����<1
2642 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2643 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2644 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2645 ''
2646 '    EndIf
2647 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2648 '    '
2649 '    '�ݒ肳�ꂽ�����ʒu�����̌������s
2650 '    While( MEndFlg% = 0 )
2651 '        '�����I���m�F�@�����I���t���O�Z�b�g
2652 '        If (MInspCnt% < MNum% ) Then
2653 '            MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2654 '        EndIf
2655 '        '
2656 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����J�n�@INSPTAST1
2657 '        If MEndFlg% = 0 Then
2658 '            M_01# = MInspGrNum%(MNum%)                          '����G�ԍ����n��
2659 '        EndIf
2660 '        M_02# = MEndFlg%                                        '�����I���t���O���n��
2661 '        M_05# = MNum%                                           '�����ԍ�(������1�`)
2662 '        '�^�X�N�@����G�ݒ�t���O���n��
2663 '        If MEndFlg% = 0 Then
2664 '            If 0 < MInspGrNum%(MNum%) Then
2665 '                M_03# = 1
2666 '            Else
2667 '                M_03# = 0
2668 '            EndIf
2669 '        Else
2670 '            M_03# = 0
2671 '        EndIf
2672 '        '�^�X�N�@�������ʊm�F�t���O���n��
2673 '        If 1 < MNum% Then
2674 '            If 0 < MInspGrNum%(MNum%-1) Then
2675 '                M_04# = 1
2676 '            Else
2677 '                M_04# = 0
2678 '            EndIf
2679 '        Else
2680 '            M_04# = 0
2681 '        EndIf
2682 '        '
2683 '        '�^�X�N�����J�n
2684 '        M_00# = 1                                               'TASK�����J�n
2685 '        '�^�X�N�����J�n�m�F
2686 '        M_Timer(1) = 0
2687 '        MExitFlg = 0
2688 '        While( MExitFlg = 0 )
2689 '            '�����J�n�����m�F
2690 '            If M_00# = 0 And M_10# = 8 Then
2691 '                MExitFlg = 1
2692 '            EndIf
2693 '            'timeout�`�F�b�N
2694 '            If 2000 < M_Timer(1) Then
2695 '                If MNgContinue% = 1 Then                        'NG���s?
2696 '                    MInspErrNumSub = 36                         '�G���[�ԍ��ݒ�36
2697 '                Else
2698 '                    MInspErrNum = 36                            '�G���[�ԍ��ݒ�36
2699 '                EndIf
2700 '                MExitFlg = 1
2701 '            EndIf
2702 '        WEnd
2703 '        '
2704 '        '�����ʒu�ֈړ��E�ړ������҂�
2705 '        If 0 = MInspErrNum Then
2706 '            If MEndFlg% = 0 Then
2707 '                Mvs PInspPos( MNum% )                           '�ړ�
2708 '            EndIf
2709 '        EndIf
2710 '        '
2711 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����I���҂��@INSPTAST1
2712 '        If 0 = MInspErrNum Then
2713 '            M_Timer(1) = 0
2714 '            MExitFlg = 0
2715 '            While( MExitFlg = 0 )
2716 '                '���������҂��i����I���j
2717 '                If M_10# = 1 Then
2718 '                    MExitFlg = 1
2719 '                EndIf
2720 '                '���������҂��i�ُ�I���j
2721 '                If M_10# = 0 Then
2722 '                    If MNgContinue% = 1 Then                    'NG���s?
2723 '                        MInspErrNumSub = M_12#                  '�G���[�ԍ��ݒ�@M12
2724 '                    Else
2725 '                        MInspErrNum = M_12#                     '�G���[�ԍ��ݒ�@M12
2726 '                    EndIf
2727 '                    MExitFlg = 1
2728 '                EndIf
2729 '                'timeout�`�F�b�N
2730 '                If 5000 < M_Timer(1) Then
2731 '                    If MNgContinue% = 1 Then                    'NG���s?
2732 '                        MInspErrNumSub = 31                     '�G���[�ԍ��ݒ�31
2733 '                    Else
2734 '                        MInspErrNum = 31                        '�G���[�ԍ��ݒ�31
2735 '                    EndIf
2736 '                    MExitFlg = 1
2737 '                EndIf
2738 '            WEnd
2739 '        EndIf
2740 '        '
2741 '        '�������ʊm�F
2742 '        If 0 = MInspErrNum Then
2743 '            If 1 < MNum% Then
2744 '                If 0 < MInspGrNum%(MNum%-1) Then                '��������?
2745 '                    If M_11# = 2 Then                           '����NG?
2746 '                        If MNgContinue% = 1 Then                'NG���s?
2747 '                            If MInspNGStepNum = 0 Then          'NG������?
2748 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2749 '                            EndIf
2750 '                            MInspErrNumSub = 32                 '�G���[�ԍ��ݒ� 32:����NG
2751 '                        Else
2752 ''                            MInspNGStepNum = MNum% - 1          '�������sNGStep�ԍ��ݒ�
2753 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2754 '                            MInspErrNum = 32                    '�G���[�ԍ��ݒ� 32:����NG
2755 '                        EndIf
2756 '                   EndIf
2757 '                EndIf
2758 '            EndIf
2759 '        EndIf
2760 '        '
2761 '        '�G���[�Ȃ猟�����f�I������̂�Loop���甲���邽�ߏI���t���O�Z�b�g
2762 '        If 0 <> MInspErrNum Then
2763 '            MEndFlg% = 1
2764 '        EndIf
2765 '        '
2766 '        '�������s�A�捞�����҂�
2767 '        If 0 = MInspErrNum Then
2768 '            If MEndFlg% = 0 Then
2769 '                If 0 < MInspGrNum%(MNum%) Then                  '��������?
2770 '                    M_Out(MOUT_IS_Insp%) = 1                    '�������s�v��on
2771 '                    '�捞�����m�F
2772 '                    M_Timer(1) = 0
2773 '                    MExitFlg = 0
2774 '                    While( MExitFlg = 0 )
2775 '                        '���������҂�
2776 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2777 '                            MExitFlg = 1
2778 '                        EndIf
2779 '                        'timeout�`�F�b�N
2780 '                        If 2000 < M_Timer(1) Then
2781 '                            If MNgContinue% = 1 Then            'NG���s?
2782 '                                MInspErrNumSub = 33             '�G���[�ԍ��ݒ�33
2783 '                            Else
2784 '                                MInspErrNum = 33                '�G���[�ԍ��ݒ�33
2785 '                            EndIf
2786 '                            MExitFlg = 1
2787 '                        EndIf
2788 '                    WEnd
2789 '                EndIf
2790 '                '
2791 '            EndIf
2792 '        EndIf
2793 '        MNum% = MNum% + 1
2794 '    WEnd
2795 '    '
2796 '    '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2797 '    If 0 < MZAxis% Then
2798 '        PCurrentPos = P_Curr                                    '���݈ʒu�擾
2799 '        PCurrentPos.Z = MZAxis%                                 'Z����ݒ�
2800 '        Mvs PCurrentPos                                         '���݈ʒu���ֈړ�
2801 '    EndIf
2802 '    '
2803 '    'NG���s������
2804 '    If MNgContinue% = 1 Then                                    'NG���s?
2805 '        MInspErrNum = MInspErrNumSub                            '�G���[�ԍ��ݒ�
2806 '    EndIf
2807 '    '
2808 '    '�߂�l�ݒ�
2809 '    If MInspErrNum = 0 Then
2810 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2811 '    Else
2812 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2813 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2814 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2815 '    EndIf
2816 '    '
2817 '*ISInspection_End
2818 'FEnd
2819 '
2820 '��InitialZoneB
2821 ''' <summary>
2822 ''' ����~��̕��A����
2823 ''' 1)���ޔ��@Z������Ɉړ�
2824 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
2825 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
2826 ''' 4)�C�j�V�����|�W�V�����ֈړ�
2827 ''' </summary>
2828 ''' <remarks>
2829 ''' Date : 2022/03/23 : N.Watanabe
2830 ''' </remarks>
2831 Function V fnInitialZoneB()
2832     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/04/26 �n��
2833 '
2834 '�p�����[�^
2835     Ovrd 5
2836 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2837 '    Cmp Pos, &B100011
2838 '
2839 '���A����J�n
2840 '
2841 '�u����Ɨ��݂͂̏ꏊ�́A�`���b�N���������
2842 *RecoveryChuckOpen
2843     PActive = P_Curr          '���݈ʒu���擾
2844     MRecoveryChuckOpen = 0    '�`���b�N����t���O ������
2845 'PMechaOnRoboSet(DVD���J�u����)�́A�`���b�N���
2846     If (PActive.X <= PMechaOnRoboSet.X + 1.0) And (PActive.X >= PMechaOnRoboSet.X -1.0) Then
2847         If (PActive.Y <= PMechaOnRoboSet.Y + 1.0) And (PActive.Y >= PMechaOnRoboSet.Y -1.0) Then
2848             If (PActive.Z <= PMechaOnRoboSet.Z + 1.0) And (PActive.Z >= PMechaOnRoboSet.Z -1.0) Then
2849                 MRecoveryChuckOpen = 1
2850             EndIf
2851         EndIf
2852     EndIf
2853 'PMechaOnRoboGet(DVD���J�󂯎��ʒu)�́A�`���b�N���
2854     If (PActive.X <= PMechaOnRoboGet.X + 1.0) And (PActive.X >= PMechaOnRoboGet.X -1.0) Then
2855         If (PActive.Y <= PMechaOnRoboGet.Y + 1.0) And (PActive.Y >= PMechaOnRoboGet.Y -1.0) Then
2856             If (PActive.Z <= PMechaOnRoboGet.Z + 1.0) And (PActive.Z >= PMechaOnRoboGet.Z -1.0) Then
2857                 MRecoveryChuckOpen = 1
2858             EndIf
2859         EndIf
2860     EndIf
2861 '
2862     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2863     M_Out(12256) = 0                           'DVD�`���b�N��OFF
2864     M_Out(12257) = 1                           'DVD�`���b�N�JON
2865 '
2866     M_20# = 0                                  'KEY���͏�����
2867     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
2868     If MRtn = 1 Then M_Out(12257) = 0          'DVD�`���b�N�JOFF
2869     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2870 '
2871     fErrorProcess(11,270,284,0)
2872     If M_20# = MNext% Then M_20# = MClear%
2873     If M_20# = MAbout% Then GoTo *RecoveryEnd
2874     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2875     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2876 '
2877     *RecoveryChuckOpenEnd
2878 '
2879 '�������@���ځA���ޔ����o���Ȃ����̑Ώ�
2880 '
2881 'PMechaOnRoboSet(Get)�`PMechaOnRoboSet(Get)_1�̃G���A�ɂ���Ƃ��́APMechaOnRoboSet_1��
2882 '�EPMechaOnRoboSet
2883 '�EPMechaOnRoboSet_1
2884 '�EPMechaOnRoboGet
2885 '�EPMechaOnRoboGet_1
2886 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2887     PActive = P_Curr                    '���݈ʒu���擾
2888     If (PActive.X >= -150) And (PActive.X <= -60) Then
2889         If (PActive.Y >= 540) And (PActive.Y <= 570) Then
2890             If (PActive.Z >= 290) And (PActive.Z <= 320) Then
2891                 Mvs PMechaOnRoboSet_1
2892                 Dly 1.0
2893             EndIf
2894         EndIf
2895     EndIf
2896 '
2897 'PMechaOnRoboSet(Get)_1�`PMechaOnRoboSet(Get)_2�̃G���A�ɂ���Ƃ��́APMechaOnRoboSet_2��
2898 '�EPMechaOnRoboSet_1
2899 '�EPMechaOnRoboSet_2
2900 '�EPMechaOnRoboGet_1
2901 '�EPMechaOnRoboGet_2
2902 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2903 '    PActive = P_Curr                    '���݈ʒu���擾
2904 '    If (PActive.X >= -90) And (PActive.X <= 50) Then
2905 '        If (PActive.Y >= 300) And (PActive.Y <= 570) Then
2906 '            If (PActive.Z >= 290) And (PActive.Z <= 430) Then
2907 '                Mov PMechaOnRoboSet_2
2908 '                Dly 1.0
2909 '            EndIf
2910 '        EndIf
2911 '    EndIf
2912 '
2913 '���ޔ�
2914     PActive = P_Curr
2915     Pmove = PActive
2916     Pmove.Z = 500           '���ޔ�����ꗥ�̍���
2917     If PActive.X < -400 Then
2918         Pmove.Z =290        '����(F)�󂯎��ʒu��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
2919     EndIf
2920     If PActive.X > 400 Then
2921         Pmove.Z =400        '�p���b�g��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
2922     EndIf
2923     If PActive.Z < Pmove.Z Then
2924         Mvs Pmove
2925     EndIf
2926     Dly 1.0
2927 'J1���ȊO��ޔ��|�W�V�����ֈړ�
2928     JActive = J_Curr
2929     Jmove = JTaihi
2930     Jmove.J1 = JActive.J1        'J1���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
2931     Jmove.J6 = JActive.J6        'J6���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
2932     Mov Jmove
2933     Dly 1.0
2934 'J1���݂̂�ޔ��|�W�V�����ֈړ�
2935     Mov JTaihi
2936     Dly 1.0
2937 '�C�j�V�����|�W�V�����ֈړ�
2938     Mov PInitialPosition
2939     Cmp Off
2940     Ovrd 100
2941 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n         '2022/04/20 �t�@���N�V�����̊O�ֈړ� �n��
2942 '    If M_In(11856) = 0 Then                 ' ��~���̂�
2943 '        M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
2944 '        MRet = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
2945 '        If MRet = 0 Then
2946 '        Else
2947 '            M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
2948 '        EndIf
2949 '    EndIf
2950     fErrorProcess(11,253,281,0)
2951 *RecoveryEnd
2952     Exit Function
2953 FEnd
2954 '
2955 '
2956 '��fnAutoScreenComment
2957 ''' <summary>
2958 ''' ���C����ʂ̓���󋵕\��
2959 ''' �R�����gD1005�̐ݒ�
2960 ''' </summary>
2961 '''<param name="McommentD1005%">�R�����gID</param>
2962 ''' <remarks>
2963 ''' Date   : 2021/07/07 : M.Hayakawa
2964 ''' </remarks>
2965 Function fnAutoScreenComment(ByVal McommentD1005%)
2966     M_Out16(12576) = McommentD1005%
2967 FEnd
2968 '
2969 '��fnRoboPosChk
2970 ''' <summary>
2971 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
2972 ''' </summary>
2973 '''<param name="MINNumber%">���͔ԍ�</param>
2974 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2975 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2976 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
2977 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
2978 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2979 ''' <remarks>
2980 ''' Date   : 2021/07/07 : M.Hayakawa
2981 ''' </remarks>
2982 Function M% fnRoboPosChk
2983     fnRoboPosChk = 0
2984     MRet = fnStepRead()
2985     '�����ʒu�łȂ��Ɣ��f�����ꍇ
2986     '�E�B���h��ʐ؊���
2987     If MRBTOpeGroupNo > 5 Then
2988         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2989         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2990         Dly 0.2
2991         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2992         Dly 1.5
2993         '
2994         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2995         '
2996         MLoopFlg% = 1
2997         While MLoopFlg% = 1
2998             '
2999             '
3000             MKeyNumber% = fnKEY_WAIT()
3001             Select MKeyNumber%
3002                 Case Is = MAbout%       '��~
3003                     M_20# = MAbout%
3004                     MLoopFlg% = -1
3005                     Break
3006                 Case Is = MNext%        '����
3007                     'MLoopFlg% = -1
3008                     Break
3009                 Case Is = MContinue%    '�p��
3010                     M_20# = MContinue%
3011                     MLoopFlg% = -1
3012                     Break
3013                 Default
3014                     Break
3015             End Select
3016         WEnd
3017     EndIf
3018     '
3019     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
3020         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3021         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
3022         Select MRBTOpeGroupNo
3023             Case Is = 5                          '�������Ȃ�
3024                 Break
3025             Case Is = 10                         '�����ʒu�֖߂�
3026                 'Mov PTEST001
3027                 Break
3028             Case Is = 15                         '�����ʒu�֖߂�
3029                 'Mov PTEST002
3030                 Dly 0.5
3031                 'Mov PTEST001
3032                 Dly 0.5
3033                 Break
3034             Default
3035                 Break
3036         End Select
3037         '
3038         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
3039         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
3040         MRBTOpeGroupNo = 5
3041         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
3042         Dly 1.0
3043         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
3044         fnRoboPosChk = 1                        '�����ʒu������s
3045         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3046     EndIf
3047     Exit Function
3048 FEnd
3049 '
3050 '��frInCheck
3051 ''' <summary>
3052 ''' �Z���T�[IN�`�F�b�N
3053 ''' </summary>
3054 '''<param name="MINNumber%">���͔ԍ�</param>
3055 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3056 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3057 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3058 ''' <remarks>
3059 ''' Date   : 2021/07/07 : M.Hayakawa
3060 ''' </remarks>
3061 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3062     M_Timer(4) = 0
3063     MloopFlg = 0
3064     While MloopFlg = 0
3065         MCrtTime& = M_Timer(4)
3066         If M_In(MINNumber%) = MCMPFLG% Then
3067             MloopFlg = 1
3068             frInCheck = 1
3069         ElseIf MCrtTime& > MTimeCnt& Then
3070             MloopFlg = 1
3071             frInCheck = 0
3072         EndIf
3073     WEnd
3074 FEnd
3075 '-----------------------------------------------
3076 '
3077 '�˂����ߋ@�ʐM�m�F
3078 '
3079 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3080 'fScewTcomChk = 0�@�F����I��
3081 '          �@�@ -1 �F�ُ�I��
3082 '-----------------------------------------------
3083 Function M% fScewTcomChk
3084 *ReCheckScewTcomChk
3085     fScewTcomChk = 0
3086     '�ʐM�m�F���M
3087     M_Out(MOUT_ScwT_ComChk%) = MOn%
3088     '�ʐM�m�F��M�ҋ@
3089 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3090     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3091     '�ʐM�m�F���M�I��
3092     M_Out(MOUT_ScwT_ComChk%) = MOff%
3093     If MRtn = 0 Then
3094         fScewTcomChk = -1
3095     EndIf
3096     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
3097  '
3098 FEnd
3099 '
3100 '
3101 '-----------------------------------------------
3102 '
3103 '�˂����ߊJ�n���M
3104 '
3105 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3106 'fScewTStart = 0�@�F����I��
3107 '          �@�@-1 �F�ُ�I��
3108 '-----------------------------------------------
3109 Function M% fScewTStart
3110     fScewTStart = 0
3111     nRet% = 0
3112     '�˂����ߊJ�n�ҋ@����M
3113 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3114     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3115     If MRtn = 0 Then nRet% = -1
3116     If MRtn = 0 Then GoTo *ScrewStartERROR      '�J�n�ł��Ȃ������ꍇ�W�����v
3117     Dly 0.1
3118     '�˂����ߊJ�n��M�𑗐M
3119     M_Out(MOUT_ScwT_ST%) = MOn%
3120     Dly 0.5
3121     'Wait M_In(MTEST_KEY%) = MOn%
3122     '�˂����ߊJ�n���M�I��
3123     M_Out(MOUT_ScwT_ST%) = MOff%
3124     '
3125 *ScrewStartERROR
3126     fScewTStart = nRet%
3127 FEnd
3128 '
3129 '
3130 '
3131 '-----------------------------------------------
3132 '
3133 '�˂����ߊ�����M
3134 '
3135 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3136 'fScewTcomChk = 0�@�F����I��
3137 '          �@ �@-1 �F�ُ�I��
3138 '-----------------------------------------------
3139 Function M% fScewTFinish
3140 *ReCheckScewTFinish
3141     fScewTFinish = 0
3142     '�˂����ߊ����ҋ@����M
3143 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3144     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3145     If MRtn = 0 Then
3146         fScewTFinish = -1
3147     EndIf
3148     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3149     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3150     Dly 0.1
3151     '�˂����ߊ�����M�𑗐M
3152     M_Out(MOUT_ScwT_FinOK%) = MOn%
3153     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3154     '�˂����ߊJ�n���M�I��
3155     M_Out(MOUT_ScwT_FinOK%) = MOff%
3156     'Wait M_In(MTEST_KEY%) = MOn%
3157     '
3158 *ScewTFinish_ErrEnd
3159 FEnd
3160 '
3161 '
3162 '-----------------------------------------------
3163 '
3164 '����xx��~��M
3165 '
3166 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3167 'fScewTCaseStop = 0�@�F����I��
3168 '          �@   �@-1 �F�ُ�I��
3169 '-----------------------------------------------
3170 Function M% fScewTCaseStop(ByVal MCase%())
3171 *ReCheckScewTCaseStop
3172     fScewTCaseStop = 0
3173     '����xx��~����M
3174     Wait M_In(MCase%(1)) = MOn%
3175     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3176     If MRtn = 0 Then
3177         fScewTCaseStop = -1
3178     EndIf
3179     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3180     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3181     Dly 0.1
3182     '����xx��~��M�𑗐M
3183     M_Out(MCase%(2)) = MOn%
3184     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3185     '�˂����ߊJ�n���M�I��
3186     M_Out(MCase%(2)) = MOff%
3187 *ScewTCaseStop_ErrEnd
3188     '
3189 FEnd
3190 '
3191 '
3192 '
3193 '��fScrewTighenRoboCheck
3194 '<summary>
3195 '�˂����{�Ď�
3196 '</summary>
3197 '<param name = "MStopNum%"> ��~�ԍ�</param>
3198 '<returns>���� 0:�˂����{�ُ�I�� 1:OK </returns>
3199 '<make>
3200 '2021/12/2 �����V��
3201 '</make>
3202 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3203     fnAutoScreenComment(503)    '��ԕ\��[�˂����{����I���҂�] 2022/04/26 �n��
3204     fScrewTighenRoboCheck = 1
3205     MScrewTighenRoboFlg% = 1    '�t���O�̏�����
3206     MCheck% = 0
3207     While MScrewTighenRoboFlg% = 1
3208         MCheck% = M_In16(11904)
3209         If M_In(MStopNum%) = 1 Then '��~�ʒu�܂ŗ�����
3210             MScrewTighenRoboFlg% = 0 '�֐��𔲂���
3211             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
3212         EndIf
3213         If MCheck% <> 0 Then
3214             fScrewTighenRoboError(MCheck%)
3215             Select M_20#
3216                 Case MAbout%            '��~�������ꂽ�ꍇ
3217                     M_Out(12869) = 1 Dly 1.0
3218                     MScrewTighenRoboFlg% = 0
3219                     fScrewTighenRoboCheck = 0   '�ُ�I��
3220                     Break
3221                 Case MNgProcess%        'NG�������ꂽ�ꍇ
3222                     M_Out(12873) = 1 Dly 1.0
3223                     MScrewTighenRoboFlg% = 0
3224                     fScrewTighenRoboCheck = 0   '�ُ�I��
3225                     Break
3226                 Case MContinue%             '���g���C�������ꂽ�ꍇ
3227                     M_20# = MClear%         'M_20#������
3228                     M_Out(12871) = 1 Dly 1.0
3229                     Break
3230                 Case MNext%                 '���ւ������ꂽ�ꍇ
3231                     M_20# = MClear%         'M_20#������
3232                     M_Out(12874) = 1 Dly 1.0
3233                     Break
3234             End Select
3235             Dly 0.5
3236         EndIf
3237     WEnd
3238 FEnd
3239 '��fScrewTighenRoboError
3240 '<summary>
3241 '�˂����{�G���[����
3242 '</summary>
3243 '<param name = "ErrorCode%"> �G���[�ԍ�</param>
3244 '<make>
3245 '2021/12/2 �����V��
3246 '</make>
3247 Function fScrewTighenRoboError(ErrorCode%)
3248     MCommentD1001 = ErrorCode% + 300
3249     fErrorProcess(11,MCommentD1001,0,0)
3250 FEnd
3251 '
3252 '��fErrorProcess
3253 '<summary>
3254 '�G���[����
3255 '</summary>
3256 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
3257 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
3258 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
3259 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
3260 '<make>
3261 '2021/11/5 �����V��
3262 '</make>
3263 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3264     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
3265     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
3266     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
3267     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
3268     MKeyNum% = 0
3269 *RETRY_ERR_PROCESS
3270      M_20# = MClear%     '������
3271 '        '�G���[�����L�q
3272         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3273 '        'GOT KEY���͑҂�
3274         MKeyNum% = fnKEY_WAIT()
3275 '        '
3276         If MKeyNum% = MAbout% Then   '��~��I�������ꍇ
3277             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3278             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3279             Break
3280          '
3281         ElseIf MKeyNum% = MContinue% Then   '�p����I�������ꍇ
3282             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3283             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3284         '
3285         ElseIf MKeyNum% = MNext% Then   '���ւ�I�������ꍇ
3286             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3287             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3288          '
3289         ElseIf MKeyNum% = MNgProcess% Then   '��~��I�������ꍇ
3290             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3291             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3292             Break
3293         '
3294         EndIf
3295         '
3296         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3297 FEnd
3298 '
3299 '��fnTorqueCheck
3300 ''' <summary>
3301 ''' �g���N�`�F�b�N����p�̃��C��
3302 ''' </summary>
3303 ''' <remarks>
3304 ''' Date   : 2021/12/21 : H.AJI
3305 ''' </remarks>'
3306 Function M% fnTorqueCheck
3307     '�g���N�`�F�b�N�����M  �����n��~
3308     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3309     '
3310     fnTorqueCheck = 0
3311     Ovrd 20
3312     Mov PInitialPosition              '�����ʒu�ړ�
3313     Accel 100 , 20
3314     Mvs PHandChange                   '�n���h�����ʒu
3315     Accel 100 , 100
3316     Ovrd 100
3317     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3318     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3319     Dly 0.2
3320     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3321     '
3322     'M6340  �g���N�`�F�b�N��M
3323     'Dly 5.0
3324     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
3325     Dly 1.0
3326     M_Out(12340) = 0
3327     '
3328     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
3329     M_Out(12835) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3330    Wait M_In(11843) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3331     M_Out(12835) = 0                         '�˂����{�g���N�`�F�b�N��ʐؑ֊���
3332     '
3333     '
3334     MLoopFlg = 1
3335     While MLoopFlg = 1
3336         '
3337 '        Mov PInitialPosition              '�����ʒu�ړ�
3338         '
3339         MKeyNumber = fnKEY_WAIT()
3340         Select MKeyNumber
3341             Case Is = 1           '��~
3342                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
3343                 Dly 1.0
3344                 M_Out(12343) = 0
3345                 Ovrd 20
3346                 'Mov PTicketRead_1
3347                 M_Out(12840) = 1          '�g���N�`�F�b�N�I��
3348                 Wait M_In(11859) = 1      '�˂����{����̏I��
3349                 M_Out(12840) = 0          '�g���N�`�F�b�N�I��
3350                 Ovrd 100
3351                 M_20# = 1
3352                 MLoopFlg = -1
3353                 Break
3354             Case Is = 2           '����
3355                 Break
3356             Case Is = 3           '�p��
3357                 Break
3358             Case Is = 4           '�g���N�`�F�b�N�J�n
3359                 M_Out(12342) = 1          '�g���N�`�F�b�N�J�n�v����M M6342
3360                 Dly 1.0
3361                 M_Out(12342) = 0
3362                 If M_In(11862) = 1 Then             '�g���N�`�F�b�J�[�m�F
3363                     fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3364                     MRet = fnScrewMTorque()           '�˂����{�p�g���N�`�F�b�N
3365                 EndIf
3366                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3367                 'MRet = fnMoveTorquePosi()
3368                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
3369                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3370                 Break
3371             Default
3372                 Break
3373         End Select
3374     WEnd
3375     '
3376     '�g���N�`�F�b�N����~���M
3377     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3378     '
3379     '���{�b�g�̈ʒu�����ɖ߂�
3380     Mvs PInitialPosition            '�C�j�V�����|�W�V����
3381     '
3382     '
3383  FEnd
3384  '
3385 '
3386 '
3387 '---------------------------
3388 '
3389 '    ���C����ʂ̕\���A��\���ݒ�
3390 '         �R�����gD1001, D1002, D1003�̐ݒ�
3391 '           MWindReSet = 0     ��ʔ�\��
3392 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
3393 '           MWindErrScr = 10    �G���[��� D1001, D1002
3394 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
3395 '
3396 '---------------------------
3397 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3398     fnMainScreenOpen = 0
3399     '
3400    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3401         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
3402     EndIf
3403     '
3404     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3405         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
3406     EndIf
3407     '
3408     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3409         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
3410     EndIf
3411     '
3412     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
3413     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
3414     Dly 0.5
3415     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
3416 FEnd
3417 '
3418 '��Main
3419 ''' <summary>
3420 ''' �g���N�`�F�b�N������
3421 ''' </summary>
3422 ''' <remarks>
3423 ''' Date   : 2021/12/21 : H.AJI
3424 ''' </remarks>'
3425 Function M% fnScrewMTorque
3426     fnScrewMTorque = 0
3427     M_Out(12838) = 1                         '�g���N�`�F�b�N�J�n1
3428     Wait M_In(11857) = 1                     '��M����
3429     M_Out(12838) = 0                         '�g���N�`�F�b�N�J�n1
3430     Dly 2.0
3431 FEnd
3432 '
3433 '
3434 '----------------------------------------------------------------
3435 'fTimeOutJudge
3436 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3437 '����
3438 'Address% = �Ď��A�h���X�ԍ�
3439 'JudgeFlg% = �ΏۃA�h���X�̐���I�����̒l
3440 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3441 '�߂�l = 0 �G���[
3442 '         1 ����I��
3443 '         2 ���g���C
3444 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3445 '�쐬��
3446 '2022/9/20 ����
3447 '----------------------------------------------------------------
3448 '
3449 Function M% fTimeOutJudge(ByVal MAddress , ByVal MJudgeFlg)
3450     fTimeOutJudge = 0
3451     MJudge% = 1
3452     MRtn = 0
3453     M_20# = MClear%
3454     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3455 *TimeOutLoop
3456     If MRtn = 1 Then GoTo *TimeOut
3457         fErrorProcess(11,202,203,0)
3458         If M_20# = MNext% Then GoTo *TimeOutLoop
3459         If M_20# = MContinue% Then MJudge% = 2
3460         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3461 *TimeOut
3462     fTimeOutJudge = MJudge%
3463 '
3464 *JUDGE_ERROR_END
3465 Exit Function
3466 FEnd
3467 '
3468 '��Main
3469 ''' <summary>
3470 ''' �g���N�`�F�b�N������
3471 ''' </summary>
3472 ''' <remarks>
3473 ''' Date   : 2021/12/21 : H.AJI
3474 ''' </remarks>'
3475 Function M% fnMoveTorquePosi
3476      fnMoveTorquePosi = 0
3477      Ovrd 50
3478     'Mov PTorquePosi000 '�g���N�`�F�b�N����ʒu�ֈړ�
3479      Mov PTorqueCheck_1 '�g���N�`�F�b�N���[�^�[���ֈړ�
3480     'Mov PTorquePosi020 '�g���N�`�F�b�N�r�b�g�W���C���g���
3481     '
3482     '
3483      '�ȉ��͈������񂪍쐬�����g���N�`�F�b�N�v���O����
3484     '
3485     Spd M_NSpd
3486 '-------------      �h���C�o�[RST
3487     M_Out(12240)=0     '�h���C�o�[OFF CCW
3488     M_Out(12241)=0     '�h���C�o�[OFF CW
3489     M_Out(12242)=0     '�h���C�o�[���� C1
3490     M_Out(12243)=0     '�h���C�o�[���� C2
3491     M_Out(12245)=0     '�v���O�������� F1/�v���O����2
3492 '---------------------------------------
3493 '---------------------------------------
3494     Fsc Off            '�͊o�Z���T�@Off  STEP1�͕s�v
3495 '--------------------------------------------------------------
3496 '--------------------------------------------------------------
3497 '[P-11]
3498 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�z
3499     Mov PTorqueCheck, -50                     ' �g���N-1�@�u���ʒu��� 50mm �ֈړ�
3500    'Mov PTorquePosi020, -10                    ' �g���N-1�@�u���ʒu��� 10mm �ֈړ�
3501     Dly 0.1
3502 '-----------------------
3503    'Cnt 0                           'Cnt����-2�@�I��
3504 '-----------------------
3505     Mov PTorqueCheck , -5                      '�g���N-1�@�u���ʒu��� 5mm �ֈړ�
3506     Dly 0.2
3507 '-----------------------
3508     M_Out(12242)=1                   '�h���C�o�[�Z�b�g C1
3509     Dly 0.1
3510     M_Out(12243)=1                   '�h���C�o�[�Z�b�g C2 (�o���N3)
3511     Dly 0.1
3512     M_Out(12245)=1                   '�v���O����2�Z�b�g F1  M�l�W
3513     Dly 0.1
3514     'M_Out(12241)=1                   '�h���C�o�[ON  CW
3515    M_Out(12241)=0                   '�h���C�o�[OFF  CW
3516     'Dly 0.1
3517 '--------------------------------
3518     Ovrd 40
3519    'Dly 0.1
3520 '--------------------------------  �l�W���ߑ��x�ݒ�
3521     Spd 14                            '���C�h 100-40 100% :Spd 12
3522     Dly 0.1
3523 '--------------------------------
3524 '--------------------------------
3525 '---------------------------------�y�˂����ߓ���z
3526 '
3527     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '�ړ����G���[���o
3528    Mvs PTorqueCheck               '�g���N�`�F�b�N�ʒu�ֈړ�
3529     Dly 0.3                          '�������҂�
3530    M_Out(12241)=1                   '�h���C�o�[ON  CW
3531 '
3532     Wait M_In(11584)=1                '����/�G���[���o
3533     Dly 0.1
3534     Spd M_NSpd
3535    'Ovrd 20
3536     If M_In(11256)=1 Then *LBL1       '�l�W�g�[�^���G���[���o
3537     Wait M_In(11257)=1                '�l�W����SC
3538 '---------------------------------
3539     Dly 0.1
3540     M_Out(12241)=0                    '�h���C�o�[OFF CW
3541     Dly 0.1
3542     M_Out(12242)=0                    '�h���C�o�[���� C1
3543     Dly 0.1
3544     M_Out(12243)=0                    '�h���C�o�[���� C2 (�o���N3)
3545     Dly 0.1
3546     M_Out(12245)=0                    '�v���O����2���� F1
3547 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�����܂Łz
3548 '
3549     Mvs PTorqueCheck,-60                       '������mov ����ύX
3550     Dly 0.1
3551 '--------------------------------------------------------------
3552    'Ovrd 80
3553 '--------------------------------------------------------------
3554 '---------------------------------------
3555 '---------------------------------------
3556 '---------------------------------------�G���[���E����
3557    *LBL1
3558    Fsc Off            '�͊o�Z���T�@Off   *STEP1�͕s�v
3559    Mvs ,-100
3560    M_Out(12241)=0     '�h���C�o�[OFF CW
3561    Dly 0.1
3562    M_Out(12242)=0     '�h���C�o�[���� C1
3563    Dly 0.1
3564    M_Out(12243)=0     '�h���C�o�[���� C2 (�o���N3)
3565    Dly 0.1
3566    M_Out(12245)=0     '�v���O�������� F1
3567 '---------------------------------------
3568 '---------------------------------------
3569 '-------------
3570    'Mov PInitPos19049
3571    Dly 0.1
3572 '
3573 '
3574 '
3575 FEnd
3576 '
3577 ''��Main
3578 ''' <summary>
3579 ''' �g������p�̃��C��
3580 ''' </summary>
3581 ''' <remarks>
3582 ''' Date   : 2021/07/07 : M.Hayakawa
3583 ''' </remarks>'
3584 Function Main
3585     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
3586     '
3587     If M_Svo=0 Then
3588         Servo On
3589     EndIf
3590     Wait M_Svo=1
3591 '�g���X�^�[�g���t�����v���p���XON (�ʃX���b�g��8����v���ɕύX�j
3592 '    M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3593 '�p�g���C�g����
3594     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
3595     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
3596     '
3597     M_20# = 0                                   'KEY���͏�����
3598 '    M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����(�ʒu�ړ�1/18����)
3599     MRet% = 0
3600 '�����ʒu�̊m�F�ƈړ�
3601 '
3602 '���A����@���s�E�����s����      2022/03/22 �n�� �쐬
3603     PActive = P_Curr                    '���݈ʒu���擾
3604     MRecoveryPass% = 0
3605     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3606         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3607             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3608                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
3609             EndIf
3610         EndIf
3611     EndIf
3612     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3613         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3614             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3615                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
3616             EndIf
3617         EndIf
3618     EndIf
3619     If (PActive.X <= PMechaOnJigGet_3.X + 1.0) And (PActive.X >= PMechaOnJigGet_3.X -1.0) Then
3620         If (PActive.Y <= PMechaOnJigGet_3.Y + 1.0) And (PActive.Y >= PMechaOnJigGet_3.Y -1.0) Then
3621             If (PActive.Z <= PMechaOnJigGet_3.Z + 1.0) And (PActive.Z >= PMechaOnJigGet_3.Z -1.0) Then
3622                 MRecoveryPass% = 1       'DVD���ʒu���ʒu�͕��A����p�X
3623             EndIf
3624         EndIf
3625     EndIf
3626     If MRecoveryPass% = 0 Then
3627        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
3628     EndIf
3629 '
3630 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n        'fnInitialZoneB�̊O�ֈړ� 2022/04/20 �n��
3631     If MopeNo <> 2 And M_In(MIN_TorqueCheck%) <> 1 Then       '�g���N�`�F�b�N�̎��͈ȉ������s���Ȃ� 2022/04/21 �n��
3632         If M_In(11856) = 0 Then                 ' ��~���̂�
3633             fnAutoScreenComment(501)            ' ��ԕ\��[�l�W���ߋ@�����^�]�J�n��] 2022/04/25 �n��
3634             M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
3635             MRet% = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
3636             If MRet% = 0 Then
3637             Else
3638                 M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
3639             EndIf
3640         EndIf
3641     EndIf
3642 '
3643 '    MRet% = fnRoboPosChk()
3644 '    If MRet% = 1 Then                           '�����ʒu�̓�����s�����ꍇ     '2022/04/20 �R�����g�A�E�g �n��
3645 '        fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  '�E�B���h���
3646 '        MKeyNumber% = fnKEY_WAIT()
3647 '        Select MKeyNumber%
3648 '            Case Is = MAbout%       '��~
3649 '                M_20# = MAbout%
3650 '                MLoopFlg% = -1
3651 '                Break
3652 '            Case Is = MNext%        '����
3653 '                'MLoopFlg = -1
3654 '                Break
3655 '            Case Is = MContinue%    '�p��
3656 '                M_20# = MContinue%
3657 '                MLoopFlg% = -1
3658 '                Break
3659 '            Default
3660 '                Break
3661 '        End Select
3662 '    EndIf
3663     '
3664     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
3665         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
3666 '�g���N�`�F�b�N
3667         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3668             MRet% = fnTorqueCheck()
3669             Break
3670         Else
3671 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F'12/21�R�����g�A�E�g(����)
3672 '                MRtn = InspInit()               '�摜��������������
3673 '            EndIf
3674             '
3675            M_20# = MClear%                    '������
3676 '�g���J�n
3677             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3678                 'MRet% = fnAssyStart()  '�b��R�����g�A�E�g
3679                 fnAssyStart()
3680             Else
3681                 M_20# = MPass%
3682             EndIf
3683             M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����(�ʒu�ړ�1/18����)
3684 '�g���I�����t����
3685             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3686             Wait M_In(11572) = 1            '���t�擾����
3687             Dly 0.1
3688             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3689 '���t�^�[���j�b�g�ւ�OUT
3690             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3691             fnAutoScreenComment(89)         'AUTO��� �g����������
3692             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3693 'OK/NG�t���O�o��
3694             If M_20# = MAssyOK% Or M_22# = MIrregular% Then
3695                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3696             ElseIf M_20# = MPass% Then
3697                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3698             EndIf
3699 'PIAS�ɑg������������
3700             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3701                 If M_20# = MPass% Then
3702                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3703                 Else
3704                     'KEY���͂�NG�̏ꍇ
3705                     If M_20# = MNgProcess% Then
3706                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3707                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3708                         MRet% = fnPiasWrite(MNG%)
3709                        nAssyNgQty = nAssyNgQty + 1
3710                     EndIf
3711                     '
3712                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(0����MAssyOK%�֕ύX1/12����)
3713                     If M_20# = MAssyOK% Or M_22# = MIrregular% Then
3714                             '-----------------------
3715                             'D732 -> D2600 �R�s�[�v��
3716                             M_Out(12566) = 1
3717 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3718                             M_Out(12566) = 0
3719                             '
3720                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3721                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3722                             '��ԍ��ƍ�(PP�͖��g�p�j
3723 '                            MRet% = fnPCBNumberCheck()
3724                         Else
3725                             MRet% = 1
3726                         EndIf
3727                         '
3728                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3729                             If M_20# <> MAbout% Then
3730                                 '�H������OK��������
3731                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3732                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3733                                 MRet% = fnPiasWrite(MOK%)
3734                                 nAssyOkQty = 0
3735                                 nAssyOkQty = nAssyOkQty + 1
3736                             Else
3737                                 nAssyOkQty = nAssyOkQty + 1
3738                             EndIf
3739                         EndIf
3740                     EndIf
3741 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3742 '                    MRet% = fnPiasWrite(MOK%)
3743                 EndIf
3744             Else
3745                 nAssyOkQty = nAssyOkQty + 1
3746             EndIf
3747             '
3748             '�g���I�����t��������
3749             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3750             '�������A�g��OK���A�g��NG��������
3751 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3752             '
3753 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F'�R�����g�A�E�g12/21(����)
3754 '                '�摜�����I������
3755 '                MRtn = InspQuit()
3756 '            EndIf
3757         EndIf
3758         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3759     EndIf
3760 '�p�g���C�g����
3761     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3762     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3763 'GOT�\��
3764     fnAutoScreenComment(93)  'AUTO��� �H������
3765 FEnd
3766 End
3767 '
3768 '
3769 '���܂��Ȃ��R�����g
3770 '��΍폜�����
3771 '
3772 '
3773 '
3774 '
3775 '
JActive=(98.220,45.650,49.730,-0.010,84.660,97.240,0.000,0.000)
Jmove=(98.220,-15.800,124.160,0.000,71.590,97.240,0.000,0.000)
JTaihi=(0.000,-15.800,124.160,0.000,71.590,0.000)
PActive=(-80.030,554.090,454.000,-179.950,0.020,-179.020,0.000,0.000)(7,0)
PBracketFCheck=(-21.450,489.130,480.000,-180.000,0.000,0.000)(7,0)
PBracketFCheck1=(2.270,435.020,540.180,145.000,-0.010,-90.000)(7,0)
PBracketFCheck2=(2.270,535.020,540.180,145.000,-0.010,-90.000)(7,0)
PBracketFCheck_2=(-171.460,546.610,480.000,-180.000,0.000,-90.000)(7,0)
PBracketFCheck_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketFCheck_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketFCheck_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketFCheck_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketFGet=(-603.300,-103.100,237.390,-179.980,0.000,91.770)(7,1)
PBracketFGet_1=(-603.300,-103.100,269.930,-179.980,0.000,91.770)(7,1)
PBracketFGet_2=(-287.410,0.110,490.000,-180.000,0.000,0.000)(7,0)
PBracketFGet_3=(-177.200,193.830,460.540,-179.990,-0.010,-85.260)(7,0)
PBracketFGet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketFGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketFGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketFSet=(-197.340,545.180,454.110,-179.360,-0.060,-88.860)(7,0)
PBracketFSet_1=(-197.340,545.180,470.000,-179.360,-0.060,-88.860)(7,0)
PBracketFSet_2=(-197.340,545.180,540.000,-179.360,-0.060,-88.860)(7,0)
PBracketFSet_3=(-26.650,244.350,539.980,-179.990,-0.010,-85.260)(7,0)
PBracketFSet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketFSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketFSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketRCheck=(-21.450,489.130,480.000,180.000,0.000,0.000)(7,0)
PBracketRCheck1=(6.080,420.000,537.590,145.000,0.000,-90.000)(7,0)
PBracketRCheck2=(6.080,550.000,537.590,145.000,0.000,-90.000)(7,0)
PBracketRCheck_2=(-177.570,546.610,479.970,-180.000,0.000,-90.000)(7,0)
PBracketRCheck_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketRCheck_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketRCheck_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketRCheck_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketRGet=(-538.990,-102.370,237.110,179.920,0.310,90.910)(7,1)
PBracketRGet_1=(-538.990,-102.370,290.000,179.920,0.310,90.910)(7,1)
PBracketRGet_2=(-287.410,0.110,490.000,-180.000,0.000,0.000)(7,0)
PBracketRGet_3=(-177.200,193.830,460.540,-179.990,-0.010,-85.260)(7,0)
PBracketRGet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketRGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketRGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketRSet=(-153.670,544.280,452.910,179.830,0.130,-88.990)(7,0)
PBracketRSet_1=(-153.670,544.280,460.000,179.830,0.130,-88.990)(7,0)
PBracketRSet_2=(-153.670,544.280,520.000,179.830,0.130,-88.990)(7,0)
PBracketRSet_3=(-26.650,244.350,539.980,-179.990,0.000,-85.260)(7,0)
PBracketRSet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketRSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PBracketRSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PHandChange=(347.780,-1.400,382.900,-90.000,89.230,-89.990)(6,0)
PInitialPosition=(303.630,-1.430,467.780,165.410,90.000,165.420)(6,0)
PInitialPosition1=(302.910,7.160,469.960,-180.000,0.000,180.000)(7,0)
PMechaOnJigGet1=(163.880,362.300,173.500,-99.950,89.010,-9.750)(6,0)
PMechaOnJigGet1_1=(163.880,362.300,200.000,-99.950,89.010,-9.750)(6,0)
PMechaOnJigGet2=(162.240,362.280,173.520,-100.800,89.010,-11.030)(6,0)
PMechaOnJigGet2_1=(162.240,362.280,200.000,-100.800,89.010,-11.030)(6,0)
PMechaOnJigGet_2=(162.750,361.540,350.000,-91.690,89.200,-1.950)(6,0)
PMechaOnJigGet_3=(160.710,268.790,438.340,-105.400,88.560,-15.270)(6,0)
PMechaOnJigGet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaOnJigGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaOnJigGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaOnPltSet=(317.130,114.460,239.200,-67.480,88.950,-67.610)(6,0)
PMechaOnPltSet_1=(317.130,114.460,280.000,-67.480,88.950,-67.610)(6,0)
PMechaOnPltSet_2=(317.130,114.460,450.000,-67.480,88.950,-67.610)(6,0)
PMechaOnPltSet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaOnPltSet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaOnPltSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaOnPltSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaOnRoboGet=(-132.250,554.570,306.140,-179.960,0.020,-179.020)(7,0)
PMechaOnRoboGet_1=(-80.000,554.570,306.140,-179.960,0.020,-179.020)(7,0)
PMechaOnRoboGet_2=(-80.000,316.960,419.500,180.000,0.000,180.000)(7,0)
PMechaOnRoboGet_3=(0.000,350.000,480.000,-180.000,0.000,-180.000)(7,0)
PMechaOnRoboGet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaOnRoboGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaOnRoboGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaOnRoboSet=(-132.250,554.570,306.140,-179.960,0.020,-179.020)(7,0)
PMechaOnRoboSet_1=(-80.000,554.570,306.140,-179.960,0.020,-179.020)(7,0)
PMechaOnRoboSet_2=(40.770,316.960,419.500,180.000,0.000,180.000)(7,0)
PMechaOnRoboSet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaOnRoboSet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaOnRoboSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaOnRoboSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
Pmove=(-80.030,554.090,500.000,-179.950,0.020,-179.020,0.000,0.000)(7,0)
PTemp=(303.630,-1.430,467.780,-0.010,90.000,0.000,0.000,0.000)(6,0)
PTicketRead=(600.000,-152.000,403.000,-180.000,0.000,90.000)(7,0)
PTicketRead_1=(600.000,-152.000,450.000,-180.000,0.000,90.000)(7,0)
PTicketRead_2=(198.380,259.830,450.000,180.000,0.000,128.470)(7,0)
PTicketRead_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(1)=(600.000,-152.000,403.000,-180.000,0.000,90.000,0.000,0.000)(7,0)
PInspPosition(2)=(6.080,550.000,537.590,145.000,0.000,-90.000,0.000,0.000)(7,0)
PInspPosition(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(11)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(12)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(13)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(14)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(15)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(16)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(17)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(18)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(19)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(20)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(21)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(22)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(23)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(24)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(25)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(26)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(27)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(28)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(29)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(30)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
