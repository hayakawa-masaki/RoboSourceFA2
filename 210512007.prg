1 ' ===================================
2 '
3 '  210512003 STEP5 Assy3�v���O����
4 '
5 ' �쐬�ҁF������T
6 ' �쐬���F2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1���痬�p
8 ' ===================================
9 '===== <Insight�萔> =====
10 '===== <Insight�ϐ���`> =====
11 Dim PInspPosition(30)               '�摜����Function���n���p�ʒu�ϐ�
12 Dim MInspGroup%(30)                 '�摜����Function���n���p�ϐ�
13 Def Inte MIN_IS_Ready               '�y����IO�zInsight����OK
14 Def Inte MIN_IS_JobLoadOK           '�y����IO�zInsight�W���u���[�h����I��
15 Def Inte MIN_IS_JobLoadNG           '�y����IO�zInsight�W���u���[�h�ُ�I��
16 Def Inte MIN_IS_InspGSetOK          '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
17 Def Inte MIN_IS_InspGSetNG          '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
18 Def Inte MIN_IS_InspOK              '�y����IO�zInsight����OK
19 Def Inte MIN_IS_InspNG              '�y����IO�zInsight����NG
20 Def Inte MIN_IS_InspErr             '�y����IO�zInsight�����ُ�I��
21 Def Inte MIN_IS_InspCapDone         '�y����IO�zInsight�����摜�捞����
22 '
23 Def Inte MIN_IS_ErrNum              '�y����IO�zInsight�����G���[�ԍ��擾�J�n�A�h���X(16bit)
24 'Output Signal
25 Def Inte MOUT_IS_JobLoadReq         '�y�o��IO�zInsight JOB���[�h�v��
26 Def Inte MOUT_IS_InspGSetReq        '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
27 Def Inte MOUT_IS_Insp               '�y�o��IO�zInsight �������s�v��
28 '
29 Def Inte MOUT_IS_JobNum             '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
30 Def Inte MOUT_IS_InspGNum           '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
31 '
32 Def Inte MOUT_InspErrNum            '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
33 Def Inte MOUT_InspNGStepNum         '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
34 '��Ɨp�ϐ�
35 Def Inte MInspErrNum                '�������s�G���[�ԍ�
36 Def Inte MInspNGStepNum             '�������sNGStep�ԍ�
37 Def Inte MRtn                       'Function�߂�l�擾�p
38 Def Inte MRtn2                      'Function�߂�l�擾�p
39 Def Inte MRet3                      'Function�߂�l�擾�p
40 Def Inte MGRtn                      'Function�߂�l�擾�p �l�W�����@
41 Def Inte MInspErrNumSub             '�������s�G���[�ԍ�sub�@20190820�ǉ�
42 Def Inte MovrdA                     '�l�W����Ovrd �ϗp
43 Def Float MSpdA                     '�l�W����Spd�@�ϗp
44 Def Pos PTemp                       '�l�W���ߏ��ʒu�v�Z�p
45 '===== <Insight�ϐ��ݒ�> =====
46 MIN_IS_Ready%        =   11380      '�y����IO�zInsight����OK
47 MIN_IS_JobLoadOK%    =   11381      '�y����IO�zInsight�W���u���[�h����I��
48 MIN_IS_JobLoadNG%    =   11382      '�y����IO�zInsight�W���u���[�h�ُ�I��
49 MIN_IS_InspGSetOK%   =   11383      '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
50 MIN_IS_InspGSetNG%   =   11384      '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
51 MIN_IS_InspOK%       =   11385      '�y����IO�zInsight����OK
52 MIN_IS_InspNG%       =   11386      '�y����IO�zInsight����NG
53 MIN_IS_InspErr%      =   11387      '�y����IO�zInsight�����ُ�I��
54 MIN_IS_InspCapDone%  =   11388      '�y����IO�zInsight�����摜�捞����
55 MIN_IS_ErrNum%       =   11408      '�y����IO�zInsight�����G���[�ԍ��J�n�A�h���X(16bit)
56 'Output Signal
57 MOUT_IS_JobLoadReq%  =   12370      '�y�o��IO�zInsight JOB���[�h�v��
58 MOUT_IS_InspGSetReq% =   12371      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
59 MOUT_IS_Insp%        =   12372      '�y�o��IO�zInsight �������s�v��
60 MOUT_IS_JobNum%      =   12384      '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
61 MOUT_IS_InspGNum%    =   12400      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
62 MOUT_InspErrNum%     =   12416      '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
63 MOUT_InspNGStepNum%  =   12432      '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
64 '===== <�d�h���ϐ���`> =====
65 X20_Driver=11248                    '�d�h���X�e�C�^�X1�@Driver Status 1
66 X21_Driver=11249 '�d�h���X�e�C�^�X2  Driver Status 2
67 X22_Driver=11250 '�d�h���X�e�C�^�X3  Driver Status 3
68 X23_Driver=11251 '�d�h���X�e�C�^�X4  Driver Status 4
69 X24_Driver=11252 '�d�h���G���[���b�Z�[�W1 Driver Error E1
70 X25_Driver=11253 '�d�h���G���[���b�Z�[�W2 Driver Error E2
71 X26_Driver=11254 '�d�h���G���[���b�Z�[�W3 Driver Error E3
72 X27_Driver=11255 '�d�h���G���[���b�Z�[�W4 Driver Error E4
73 X28_Driver=11256 '�d�h���g�[�^���G���[�V�O�i�� Total Error
74 X29_Driver=11257 '�d�h���I���V�O�i�� Comlete signal
75 X2A_Driver=11258 '�d�h���G���[���b�Z�[�W5 Driver Error E5
76 '11584   'toRB�g���N�h���C�o-COMP_ERR���M
77 Y60_Driver=12240 '�d�h�������v��� CCW
78 Y61_Driver=12241 '�d�h�����v��� CW
79 Y62_Driver=12242 '�o���N�Z�b�e�B���O BANK C1
80 Y63_Driver=12243 '�o���N�Z�b�e�B���O BANK C2
81 Y64_Driver=12244 '�o���N�Z�b�e�B���O BANK C3
82 Y65_Driver=12245 '�v���O�����Z�b�e�B���O PRG SET F1
83 Y66_Driver=12246 '�v���O�����Z�b�e�B���O PRG SET F2
84 Y67_Driver=12247 '�v���O�����Z�b�e�B���O PRG SET F3
85 X34_ScrewReady1=11259 '�˂�����1�@Read
86 '===== <�d�h���萔> =====
87 Dim PScrewPos(10)       '�l�W���ߗpFunction�����ϐ�
88 Dim PGetScrewPos(10)    '�˂������@����˂��𓾂�Function�����ϐ�
89 Dim PEscapePosi(10)
90 MLoopCnt% = 0'
91 '===== <���{�b�g�萔> =====
92 '===== <���{�b�g�ϐ���`> =====
93 MRBTOpeGroupNo = 0      '���{�b�g����ԍ�������
94 MCommentD1001 = 0
95 MCommentD1002 = 0
96 MCommentD1003 = 0
97 MScreenNo = 0
98 '
99 MCommentTSU = 0
100 MCommentTSD = 0
101 '�E�B���h��ʔԍ��ݒ�
102 MWindReSet = 0
103 MWindInfoScr = 5
104 MWindErrScr = 10
105 MWindErrScr2 = 11
106 MWindErrScr3 = 13
107 MWindErrScr17 = 17
108 MWindErrScr18 = 18
109 MWindCmmnScr = 20
110 MWindJigRelase19049 = 60
111 MWindJigRelase19050 = 61
112 MWindJigRelase19051 = 62
113 '
114 MClear% = 0        'KEY_�̃N���A
115 MAbout% = 1        'KEY_��~
116 MNext% = 2         'KEY_���̃X�e�b�v�ֈڍs
117 MContinue% = 3     'KEY_�p�� �ēx����������s��
118 '
119 Def Inte MNgProcess
120 MNgProcess% = 5      'KEY_NG
121 '
122 MAssyOK% = 6       '�g������
123 MPass% = 7         '�H���p�X
124 MPiasNG% = 8       'Pias�m�F������NG
125 '
126 '�������pKEY�ԍ�   '
127 MRobotInit1% = 11  '�����ʒu�p
128 MRobotInit2% = 12  '�����ʒu�p
129 MRobotInit3% = 13  '�����ʒu�p
130 MRobotInit4% = 14  '�����ʒu�p
131 '
132 MIN_INIT1REQUEST% = 11568 'toRBT_���{�b�g�����ʒu1�v��
133 MIN_INIT2REQUEST% = 11569 'toRBT_���{�b�g�����ʒu2�v��
134 MIN_INIT3REQUEST% = 11570 'toRBT_���{�b�g�����ʒu3�v��
135 MIN_INIT4REQUEST% = 11571 'toRBT_���{�b�g�����ʒu4�v��
136 '
137 MOUT_INIT1RECIVE% = 12560 'toPLC_���{�b�g�����ʒu1��M
138 MOUT_INIT2RECIVE% = 12561 'toPLC_���{�b�g�����ʒu2��M
139 MOUT_INIT3RECIVE% = 12562 'toPLC_���{�b�g�����ʒu3��M
140 MOUT_INIT4RECIVE% = 12563 'toPLC_���{�b�g�����ʒu4��M
141 '
142 MOK% = 1               '�e����p
143 MNG% = 0               '�e����p
144 MTIMEOUT% = -1         '�e����p
145 MJudge% = 0            '������i�[�p
146 '
147 MRECIVETIME& = 0
148 MSETTIMEOUT10& = 10000&                '10�b�ݒ�
149 MSETTIMEOUT03& = 3000&                 '3�b�ݒ�
150 MSETTIMEOUT01& = 1000&                 '1�b�ݒ�
151 MSETTIMEOUT05& = 5000&                 '5�b�ݒ�
152 MSETTIMEOUT009& = 900&                 '0.9�b�ݒ�
153 MSETTIMEOUT008& = 800&                 '0.8�b�ݒ�
154 MSETTIMEOUT007& = 700&                 '0.7�b�ݒ�
155 MSETTIMEOUT006& = 600&                 '0.6�b�ݒ�
156 MSETTIMEOUT005& = 500&                 '0.5�b�ݒ�
157 MSETTIMEOUT004& = 400&                 '0.4�b�ݒ�
158 MSETTIMEOUT003& = 300&                 '0.3�b�ݒ�
159 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
160 MIN_PIAS_ComOK% = 11552                'PC�ʐMOK
161 MIN_PIAS_ComTimeOut% = 11576           'PC�ʐM�m�F�^�C���A�E�g
162 MIN_PIAS_ComNG% = 11553                'PC�ʐMNG
163 MOUT_PIAS_ComCheck% = 12544            'PC�ʐM�m�F�v��
164 MOUT_PIAS_Missing_Process% = 12546     '�H�������m�F�v��
165 MIN_PIAS_ModelTypeNG% = 11554          '���f���d��NG
166 MIN_PIAS_ProcessHistryNG% = 11555      '�O�H������NG
167 MIN_PIAS_ProcessHistryOK% = 11556      '�O�H������OK
168 MIN_PIAS_ProcessHistryErr% = 11557     '�H�����������G���[
169 MIN_PIAS_MyProcessComp% = 11573        '���H����������
170 MIN_PIAS_ProcessHistryTimeOut% = 11578 '�H�������^�C���A�E�g
171 MOUT_OKNG% = 12549                     'PLC OUT ��OK=1, NG=0 �o��
172 '
173 MOUT_PiasPCBNumberCheck = 12557        '��ԍ��ƍ�
174 MIN_PiasPCBNumberOK% = 11566          '��ԍ�OK
175 MIN_PiasPCBNumberNG% = 11565          '��ԍ�NG
176 MIN_PiasPCBNumberErr% = 11567         '��ԍ������G���[
177 '
178 MOUT_PiasAssyResultOK% = 12549    '�g��OK
179 MOUT_PiasAssyResultNG% = 12550    '�g��NG
180 MOUT_PiasAssyResultWr% = 12548    '�H��������������
181 '
182 MIN_PiasProcessNG% = 11559        '�H����������NG
183 MIN_PiasProcessOtherErr% = 11560  '�H�����������G���[(�Ȃ񂩂̃g���u��)
184 MIN_PiasProcessOK% = 11558        '�H����������OK
185 '
186 MIN_Insight_Use% = 11369               '�摜�m�FON
187 MIN_TorqueCheck% = 11348               '�g���N�`�F�b�N
188 '
189 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT���쌠
190 MOUT_RED_LIGHT% = 12356            'PATLIGHT �� �_��
191 MOUT_RED_FLASH% = 12357            'PATLIGHT �� �_��
192 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT �� �_��
193 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT �� �_��
194 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT �� �_��
195 MOUT_GREEN_FLASH% = 12361          'PATLIGHT �� �_��
196 '
197 MOUT_ST_DATETIME% = 12551          '�g���J�n���t����
198 MOUT_ED_DATETIME% = 12552          '�g���I�����t����
199 '
200 MOUT_TORQUE_CHECK% = 12367         'PLC�փg���N�`�F�b�N���𑗐M
201 '
202 MIN_ASSY_CANCEL% = 11366           '�g�����s�����̃t���O
203 '
204 MLoopFlg% = 0                      'KEY���͌��OK or NG���e
205 MopeNo% = 0
206 MRtn% = 0
207 MRet = 0
208 MRet3% = 0
209 '
210 Def Inte MInputQty          '������ ���Z�ϐ�
211 Def Inte MAssyOkQty         '�g���n�j�� ���Z�ϐ�
212 Def Inte MAssyNgQty         '�g���m�f�� ���Z�ϐ�(���g�p)
213 Def Inte MSuctionErrQty     '�z���G���[�� ���Z�ϐ� 2022/04/27 �n��
214 Def Inte nAssyOkQty         '���g�p
215 Def Inte MScrewNo
216 Def Inte MReTry
217 '===== <IO�ϐ���`> =====
218 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
219 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
220 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
221 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
222 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
223 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
224 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
225 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
226 '
227 Def Inte Y68_VV1            ' �A�[����[�@�l�W�z���o���u
228 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
229 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
230 '
231 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
232 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
233 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
234 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
235 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
236 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
237 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
238 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
239 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
240 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
241 '
242 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
243 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
244 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
245 '
246 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
247 '
248 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
249 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
250 '
251 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
252 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
253 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
254 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
255 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
256 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
257 '
258 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
259 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
260 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
261 '
262 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
263 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
264 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
265 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
266 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
267 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
268 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
269 Y68_VV1%    =  12248    ' �A�[����[�@�l�W�z���o���u '���l12250����12248�֕ύX(8/5����)
270 Y6B_VB1%    =  12250    '�A�[����[�@�z���j��o���u  '���l12251����12250�֕ύX(8/5����)
271 MOUT_VB1%   =  12250    ' �A�[����[�@�l�W�z���j��o���u  '���l12251����12250�֕ύX(8/5����)
272 '
273 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
274 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
275 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
276 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
277 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
278 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
279 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
280 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
281 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
282 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
283 '
284 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
285 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
286 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
287 '
288 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
289 '
290 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
291 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
292 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
293 '
294 '����
295 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
296 Def Inte MOn                            '�o��=1
297 Def Inte MOff                           '�o��=0
298 '
299 '�˂����ߑ��u_�o�̓A�h���X
300 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
301 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
302 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
303 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
304 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
305 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
306 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
307 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
308 '�˂����ߑ��u_���̓A�h���X
309 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
310 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
311 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
312 Def Inte MIN_ScwT_Case1                 '����1��~����M
313 Def Inte MIN_ScwT_Case2                 '����2��~����M
314 Def Inte MIN_ScwT_Case3                 '����3��~����M
315 Def Inte MIN_ScwT_Case4                 '����4��~����M
316 Def Inte MIN_ScwT_Case5                 '����5��~����M
317 '
318 Dim MScwT_Case1%(2)               '����1��~�ϐ�
319 Dim MScwT_Case2%(2)               '����2��~�ϐ�
320 Dim MScwT_Case3%(2)               '����3��~�ϐ�
321 Dim MScwT_Case4%(2)               '����4��~�ϐ�
322 Dim MScwT_Case5%(2)               '����5��~�ϐ�
323 '
324 '����
325 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
326 MOn% = 1                                 '�o�� = 1
327 MOff% = 0                                '�o�� = 0
328 '
329 '�˂����ߋ@_�A�h���X�ݒ�
330 MOUT_ScwT_ComChk% = 12832               '�ʐM�m�F���M
331 MOUT_ScwT_ST% = 12865                   '�˂����ߊJ�n�𑗐M
332 MOUT_ScwT_ReSTOK% = 12866               '�ĊJ�n��M�𑗐M
333 MOUT_ScwT_FinOK% = 12868                '�˂����ߊ�����M�𑗐M
334 MOUT_ScwT_Case1OK% = 12874              '����1��~��M�𑗐M
335 MOUT_ScwT_Case2OK% = 12875              '����2��~��M�𑗐M
336 MOUT_ScwT_Case3OK% = 12876              '����3��~��M�𑗐M
337 MOUT_ScwT_Case4OK% = 12877              '����4��~��M�𑗐M
338 MOUT_ScwT_Case5OK% = 12878              '����5��~��M�𑗐M
339 '
340 MIN_ScwT_comOK% = 11840                 '�˂����ߑ��u����ԐM
341 MIN_ScwT_STRec% = 11873                 '�˂����ߊJ�n����M
342 MIN_ScwT_ReST% = 11874                  '�ĊJ�n����M
343 MIN_ScwT_Fin% = 11876                   '�˂����ߊ�������M
344 MIN_ScwT_Case1% = 11882                 '����1��~�ҋ@����M
345 MIN_ScwT_Case2% = 11883                 '����2��~�ҋ@����M
346 MIN_ScwT_Case3% = 11884                 '����3��~�ҋ@����M
347 MIN_ScwT_Case4% = 11885                 '����4��~�ҋ@����M
348 MIN_ScwT_Case5% = 11886                 '����5��~�ҋ@����M
349 '
350 MScwT_Case1%(1) = MIN_ScwT_Case1%
351 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
352 MScwT_Case2%(1) = MIN_ScwT_Case2%
353 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
354 MScwT_Case3%(1) = MIN_ScwT_Case3%
355 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
356 MScwT_Case4%(1) = MIN_ScwT_Case4%
357 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
358 MScwT_Case5%(1) = MIN_ScwT_Case5%
359 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
360 '
361 '�ݒ� InitialZoneB�Ŏg�p����ϐ�
362 Def Pos PActive       '�������W�n �ʒu�ϐ� ���݈ʒu
363 Def Pos Pmove         '�������W�n �ʒu�ϐ� �ړ���
364 Def Jnt JActive       '�֐ߍ��W�n �ʒu�ϐ� ���݈ʒu
365 Def Jnt Jmove         '�֐ߍ��W�n �ʒu�ϐ� �ړ���
366 Def Jnt JTaihi        '�֐ߍ��W�n �ʒu�ϐ� �ޔ��|�W�V���� �e�B�[�`���O�Őݒ�
367 Def Inte MRecoveryPass      '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s
368 Def Inte MStandby              '�ҋ@�ʒu�m�F�t���O
369 Def Inte MRecoveryChuckOpen    '�`���b�N����t���O�i���A����O�j���͂ݑ΍�
370 '�����Ӂ������ʒu��ύX�������ɂ́A�ύX���K�v�I
371 '
372 '�G���[�΍�
373 Def Inte MCtlNo    'Function�G���[�΍� 2022/04/15 �n��
374 '
375 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
376 Function M% fnAssyStart
377     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/27 �n��
378     M_20# = MClear%                       '������
379 ''    '�˂����ߊJ�n(�����ʒu�ύX2/16����)
380 '    fScewTStart()
381 '
382 ' �g���J�n
383 '�v���O�������_            '(�ǉ���������(8/30����))
384 Ovrd 100
385 '
386 '�n���h�ɖ{��,�����Ȃ����m�F
387 *RE_INITIAL_CHECK
388 If M_20# = MContinue% Then M_20# = MClear%
389 '
390 If M_In(11264) = 0 Then GoTo *CompInitial_1
391 fErrorProcess(11,253,281,0)
392 If M_20# = MNext% Then M_20# = MClear%
393 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
394 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
395 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
396 *CompInitial_1
397 '
398 If M_In(11267) =0 And M_In(11272)= 0 Then GoTo *CompInitial_2
399 fErrorProcess(11,255,281,0)
400 If M_20# = MNext% Then M_20# = MClear%
401 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
402 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
403 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
404 *CompInitial_2
405 '
406 '�n���h���C�j�V�����ɖ߂�
407 If M_In(11266) = 1 Then             '�{�̃`���b�N���o
408     M_Out(12256) = 0                '�{�̃`���b�N��OFF
409     M_Out(12257) = 1                '�{�̃`���b�N�JON
410     Break
411 EndIf
412 If M_In(11269) = 1 Then             '����L�V�����_�[�o���o
413     M_Out(12258) = 0                '����L�V�����_�[�oOFF
414     M_Out(12259) = 1                '����L�V�����_�[��ON
415     Break
416 EndIf
417 If M_In(11274) = 1 Then             '����R�V�����_�[�o���o
418     M_Out(12260) = 0                '����R�V�����_�[�oOFF
419     M_Out(12261) = 1                '����R�V�����_�[��ON
420     Break
421 EndIf
422 M_Out(12262) = 0                    '���`���b�N��OFF
423 M_Out(12263) = 1                    '���`���b�N�JON
424 '
425 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)'�{�̃`���b�N�J���o
426 If MRtn = 1 Then GoTo *CompInitial_3
427 fErrorProcess(11,244,281,0)
428 If M_20# = MNext% Then M_20# = MClear%
429 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
430 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
431 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
432 *CompInitial_3
433 '
434 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)'����L�ߌ��o
435 If MRtn = 1 Then GoTo *CompInitial_4
436 fErrorProcess(11,247,281,0)
437 If M_20# = MNext% Then M_20# = MClear%
438 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
439 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
440 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
441 *CompInitial_4
442 '
443 MRtn = frInCheck(11273,1,MSETTIMEOUT05&)'����R�ߌ��o
444 If MRtn = 1 Then GoTo *CompInitial_5
445 fErrorProcess(11,249,281,0)
446 If M_20# = MNext% Then M_20# = MClear%
447 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
448 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
449 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
450 *CompInitial_5
451 '
452 '�����ʒu��ݒ�
453 PTemp = P_Curr
454 MRtn = 0
455 'If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
456 '    If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
457 '        If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
458 '            MRtn = 1
459 '            Break
460 '        EndIf
461 '        Break
462 '    EndIf
463 '    Break
464 'EndIf
465 'If MRtn = 1 Then
466 '    M_Out(12265) = 0            '�ʒu���ߖ�OFF
467 '    M_Out(12264) = 1            '�ʒu���ߏoON
468 '    Mov PTicketRead
469 '    Break
470 'Else
471 '    Mov PInitialPosition
472 '    M_Out(12265) = 0            '�ʒu���ߖ�OFF
473 '    M_Out(12264) = 1            '�ʒu���ߏoON
474 '    Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
475 '    Mvs PTicketRead             'ID�ǂ݈ʒu
476 '    Break
477 'EndIf
478 '
479 ' 2022/04/12 ���S�����֏����ύX �n��
480 ' PInitialPosition �ݐ� MStandby=2
481 ' PTicketRead_1 �ݐ� MStandby=1
482 '
483 MStandby = 0    '�ҋ@�ʒu�t���O��������
484 If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
485     If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
486         If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
487             MStandby = 2
488         EndIf
489     EndIf
490 EndIf
491 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
492     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
493         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
494             MStandby = 1
495         EndIf
496     EndIf
497 EndIf
498 If MStandby = 2 Then
499     M_Out(12265) = 0            '�ʒu���ߖ�OFF
500     M_Out(12264) = 1            '�ʒu���ߏoON
501     Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
502     Mvs PTicketRead             'ID�ǂ݈ʒu
503     Break
504 EndIf
505 If MStandby = 1 Then
506     M_Out(12265) = 0            '�ʒu���ߖ�OFF
507     M_Out(12264) = 1            '�ʒu���ߏoON
508     Mvs PTicketRead             'ID�ǂ݈ʒu
509     Break
510 EndIf
511 If MStandby <> 0 Then GoTo *PositionOK
512 fErrorProcess(11,230,281,0)           '�����ʒu�ɂ��Ȃ����̓G���[�ɂ���
513 If M_20# = MNext% Then GoTo *ASSY_ERROR_END
514 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
515 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
516 If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
517 *PositionOK
518 '
519 MRtn = 1        'MRtn������
520 '�`�P�b�gID��ǂ�
521 *RE_TICKET_READ
522 M_20# = MClear%                       '������
523 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
524     MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
525     '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
526     '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
527 EndIf
528 If MRtn = 1 Then GoTo *CompRead
529 'fErrorProcess(11,244,284,0)
530 If M_20# = MNext% Then M_20# = MClear%
531 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
532 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
533 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
534 'If M_20# = MNext% Then M_20# = MPass%
535 Mov PTicketRead_1
536 Mov PInitialPosition
537 GoTo *ASSY_ERROR_END
538 *CompRead
539 Mov PTicketRead_1               '�`�P�b�gID�ǂݎ����_
540 'Dly 10                   '�f�o�b�O�p(22/09/30����)
541 '    '�˂����ߊJ�n(�����ʒu�ύX2/16����)
542     MRtn2 = fScewTStart()
543     If MRtn2 = 0 Then GoTo *INITIAL_CHECK
544         fErrorProcess(11,329,201,0)
545         If M_20# = MNext% Then GoTo *CompRead
546         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
547         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
548         If M_20# = MContinue% Then GoTo *CompRead
549 '
550 '
551 *INITIAL_CHECK
552 '
553 '�p���b�g���琻�i�����
554 Mov PProductOnPltGet_2      '�{�̉��_
555 '
556 *RE_PLT_GET_1
557 '
558 If M_20# = MContinue% Then M_20# = MClear%
559 '
560 M_Out(12256) = 0            '�{�̃`���b�N��OFF
561 M_Out(12257) = 1            '�{�̃`���b�N�JON
562 '
563 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)        '�{�̃`���b�N�J�Z���T�[ON
564 If MRtn = 1 Then GoTo *CompPltGet_1
565 fErrorProcess(11,244,284,0)
566 If M_20# = MNext% Then M_20# = MClear%
567 If M_20# = MAbout% Or M_20# = MNgProcess% Then
568     Mov PInitialPosition
569     M_Out(12264) = 0            '�ʒu���ߏoOFF
570     M_Out(12265) = 1            '�ʒu���ߖ�ON
571     Break
572 EndIf
573 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
574 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
575 If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
576 *CompPltGet_1
577 '
578 'Mov PProductOnPltGet_1      '�{�̏��(�����ʒu�ύX1/21����)
579 'Wait M_In(11278) = 1        '�ʒu���ߏo�[���o(�R�����g�A�E�g11/4����)
580 MRtn = frInCheck(11278,1,MSETTIMEOUT05&)        '�ʒu���ߏo�[���o
581 If MRtn = 1 Then GoTo *CompPltGet_2
582 fErrorProcess(11,231,282,0)
583 If M_20# = MNext% Then M_20# = MClear%
584 If M_20# = MAbout% Or M_20# = MNgProcess% Then
585     Mov PProductOnPltGet_2
586     Mov PInitialPosition
587     M_Out(12264) = 0            '�ʒu���ߏoOFF
588     M_Out(12265) = 1            '�ʒu���ߖ�ON
589     Break
590 EndIf
591 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
592 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
593 If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
594 *CompPltGet_2
595 '
596 Mov PProductOnPltGet_1      '�{�̏��(�����ʒu�ύX1/21����)
597 '
598 M_Out(12264) = 0            '�ʒu���ߏoOFF
599 M_Out(12265) = 1            '�ʒu���ߖ�ON
600 Ovrd 25
601 Mvs PProductOnPltGet        '�{�̂����ʒu
602 '
603 *RE_PLT_GET_2
604 '
605 If M_20# = MContinue% Then M_20# = MClear%
606 '�ʒu���ߖ߂̃G���[����
607 MRtn = frInCheck(11279,1,MSETTIMEOUT05&)    '�ʒu���ߖߒ[���o
608 If MRtn = 1 Then GoTo *CompPushIni
609 fErrorProcess(11,234,284,0)
610 If M_20# = MNext% Then M_20# = MClear%
611 If M_20# = MAbout% Or M_20# = MNgProcess% Then
612     Mvs PProductOnPltGet_1
613     Mov PProductOnPltGet_2
614     Mov PInitialPosition
615 EndIf
616 If M_20# = MAbout% Or M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
617 If M_20# = MContinue% Then
618     M_Out(12265) = 0            '�ʒu���ߖ�OFF
619     M_Out(12264) = 1            '�ʒu���ߏoON
620     Dly 1.0
621     M_Out(12264) = 0                            '�ʒu���ߏoOFF
622     M_Out(12265) = 1                            '�ʒu���ߖ�ON
623 EndIf
624 If M_20# = MContinue% Then GoTo *RE_PLT_GET_2
625 *CompPushIni
626 '
627 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
628 M_Out(12256) = 1            '�{�̃`���b�N��ON
629 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '�{�̃`���b�N�Z���T�[ON
630 If MRtn = 1 Then GoTo *CompPltGet_3
631 M_Out(12256) = 0            '�{�̃`���b�N��OFF
632 M_Out(12257) = 1            '�{�̃`���b�N�JON
633 Dly 2.0
634 Mvs PProductOnPltGet_1
635 Mov PProductOnPltGet_2
636 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
637 M_Out(12256) = 1            '�{�̃`���b�N��ON
638 fErrorProcess(11,244,284,0)
639 If M_20# = MNext% Then M_20# = MClear%
640 If M_20# = MAbout% Or M_20# = MNgProcess% Then
641     Mov PInitialPosition
642     Break
643 EndIf
644 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
645 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
646 M_Out(12256) = 0            '�{�̃`���b�N��OFF
647 M_Out(12257) = 1            '�{�̃`���b�N�JON
648 Dly 2.0
649 Mov PProductOnPltGet_1
650 Mvs PProductOnPltGet
651 If M_20# = MContinue% Then GoTo *RE_PLT_GET_2
652 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
653 M_Out(12256) = 1            '�{�̃`���b�N��ON
654 Dly 2.0
655 *CompPltGet_3
656 '
657 MRth = frInCheck(11264,1,MSETTIMEOUT05&)        '�{�̌��o�Z���T�[ON
658 If MRtn = 1 Then GoTo *CompPltGet_4
659 M_Out(12256) = 0            '�{�̃`���b�N��OFF
660 M_Out(12257) = 1            '�{�̃`���b�N�JON
661 Dly 2.0
662 Mvs PProductOnPltGet_1
663 Mov PProductOnPltGet_2
664 fErrorProcess(11,252,284,0)
665 If M_20# = MNext% Then M_20# = MClear%
666 If M_20# = MAbout% Or M_20# = MNgProcess% Then
667     Mov PInitialPosition
668     Break
669 EndIf
670 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
671 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
672 Mov PProductOnPltGet_1
673 Mvs PProductOnPltGet
674 If M_20# = MContinue% Then GoTo *RE_PLT_GET_2
675 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
676 M_Out(12256) = 1            '�{�̃`���b�N��ON
677 Dly 2.0
678 *CompPltGet_4
679 '
680     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
681 Mvs PProductOnPltGet_1          '�{�̏��
682     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
683 '
684 'Ovrd 100
685 Moverride = 2000 / M_OPovrd
686 If Moverride > 100 Then Moverride = 100
687 Ovrd Moverride
688 '
689 Mov PProductOnPltGet_2      '�{�̉��_
690 '
691 '���i���˂����{1�ɒu��
692 Mov PProductOnRoboSet_2     '�˂����{1���_
693 'Wait M_In(11888) = 1        '�˂����{1��~1��M
694 MScrewRoboNgFlg% = 0
695 '
696 MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
697 '
698 If MRtn = 0 Then MScrewRoboNgFlg% = 1
699 If MScrewRoboNgFlg% = 1 Then GoTo *ProductOnPltSet
700 Mov PProductOnRoboSet_1     '�˂����{1���
701 Ovrd 25
702 Mvs PProductOnRoboSet       '�{�̒u���ʒu
703 *RE_ROBO_SET
704 If M_20# = MContinue% Then M_20# = MClear%
705 '
706 Dly 0.3
707 M_Out(12256) = 0            '�{�̃`���b�N��OFF
708 M_Out(12257) = 1            '�{�̃`���b�N�JON
709 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)             '�{�̃`���b�N�J�Z���T�[ON
710 If MRtn = 1 Then GoTo *CompRoboSet
711 fErrorProcess(11,244,284,0)
712 If M_20# = MNext% Then M_20# = MClear%
713 If M_20# = MAbout% Or M_20# = MNgProcess% Then
714     MScrewRoboNgFlg% = 1
715     Mvs PProductOnRoboSet_1
716     Mov PProductOnRoboSet_2
717     Break
718 EndIf
719 If M_20# = MAbout% Then GoTo *ProductOnPltSet    '�O�̂��ߐ��i��u���ɍs��������s��
720 If M_20# = MNgProcess% Then GoTo *ProductOnPltSet    '�O�̂��ߐ��i��u���ɍs��������s��
721 If M_20# = MContinue% Then GoTo *RE_ROBO_SET
722 *CompRoboSet
723 '
724 Mvs PProductOnRoboSet_1     '�˂����{1���
725 Ovrd 100
726 Mvs PProductOnRoboSet_2     '�˂����{1���_
727 M_Out(12866) = 1 Dly 0.5    '�˂����{1����ĊJ(��~1�`��~2)(�����ʒu�ύX1/21����)
728 '
729 '
730 '����L���p���b�g������
731 Mov PPlateLGet_2            '����L�����_
732 'M_Out(12866) = 1 Dly 0.5    '�˂����{1����ĊJ(��~1�`��~2)(�����ʒu�ύX1/21����)
733 *RE_PLATE_L_GET_1
734 If M_20# = MContinue% Then M_20# = MClear%
735 '
736 M_Out(12257) = 0            '�{�̃`���b�N�JOFF(�ȉ�3�s,���󂯎�莞�ז��ɂȂ邽��)
737 M_Out(12256) = 1            '�{�̃`���b�N��ON
738 '
739 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '�{�̃`���b�N�Z���T�[ON
740 If MRtn = 1 Then GoTo *CompPlateLGet_1
741 fErrorProcess(11,245,284,0)
742 If M_20# = MNext% Then M_20# = MClear%
743 If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
744 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
745 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
746 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_1
747 *CompPlateLGet_1
748 '
749 Mov PPlateLGet_1            '����L�����
750 Ovrd 10
751 M_Out(12259) = 0            '����L�V�����_�[��OFF
752 M_Out(12258) = 1            '����L�V�����_�[�oON
753 MRtn = frInCheck(11269,1,MSETTIMEOUT05&)        '����L�V�����_�[�o�[���o�Z���T�[ON
754 If MRtn = 1 Then GoTo *CompPlateLGet_2
755 fErrorProcess(11,246,284,0)
756 If M_20# = MNext% Then M_20# = MClear%
757 If M_20# = MAbout% Or M_20# = MNgProcess% Then
758     Mov PPlateLGet_2
759     Mov PInitialPosition
760 EndIf
761 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
762 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
763 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_1
764 *CompPlateLGet_2
765 '
766 *RE_PLATE_L_GET_2
767 If M_20# = MContinue% Then M_20# = MClear%
768 '
769 M_Out(12262) = 0            '���`���b�N��OFF
770 M_Out(12263) = 1            '���`���b�N�JON
771 Fine 0.05 , P               '����0.05[mm]�ȓ�
772 Mvs PPlateLGet              '����L�����ʒu
773 Fine 0 , P                  'Fine����
774 M_Out(12263) = 0            '���`���b�N�JOFF
775 M_Out(12262) = 1            '���`���b�N��ON
776 'Wait M_In(11271) = 1        '���`���b�N���o�Z���T�[ON
777 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)        '����L�`���b�N���o�Z���T�[ON
778 Dly 0.5
779 Mvs PPlateLGet_1            '����L�����
780 If MRtn = 1 Then GoTo *CompPlateLGet_3
781 fErrorProcess(11,250,292,0) '284��292�ɕύX(6/7����)
782 If M_20# = MNext% Then M_20# = MClear%
783 If M_20# = MAbout% Or M_20# = MNgProcess% Then
784     Mov PPlateLGet_2
785     Mov PInitialPosition
786 EndIf
787 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
788 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
789 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_2
790 *CompPlateLGet_3
791 '
792 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)         '����L���o
793 If MRtn = 1 Then GoTo *CompPlateLGet_4
794 fErrorProcess(11,254,292,0) '284��292�ɕύX(6/7����)
795 If M_20# = MNext% Then M_20# = MClear%
796 If M_20# = MAbout% Or M_20# = MNgProcess% Then
797     Mov PPlateLGet_2
798     Mov PInitialPosition
799 EndIf
800 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
801 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
802 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_2
803 *CompPlateLGet_4
804 'Dly 5                      'test(�b��R�����g�A�E�g)
805 'M_Out(12256) = 0            '�{�̃`���b�N��OFF(���󂯎�莞�ז��ɂȂ邽��)�ȉ�3�s�b��폜(11/17����)
806 'M_Out(12257) = 1            '�{�̃`���b�N�JON(���󂯎�莞�ז��ɂȂ邽��)
807 'MRtn = frInCheck(11265,1,MSETTIMEOUT05&)         '�{�̃`���b�N�J�Z���T�[ON
808 'If MRtn = 0 Then
809 '    fErrorProcess()         '�G���[����
810 'EndIf
811 'Ovrd 100
812 Mov PPlateLGet_2            '����L�����_
813 Ovrd 100
814 '
815 '����L��u��
816 Mov PPlateLSet_2            '����L�u�����_
817 '
818 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)         '������x����L���o
819 If MRtn = 1 Then GoTo *CompPlateLGet_5
820 fErrorProcess(11,254,292,0) '284��292�ɕύX(6/7����)
821 If M_20# = MNext% Then M_20# = MClear%
822 If M_20# = MAbout% Or M_20# = MNgProcess% Then
823     Mov PPlateLGet_2
824     Mov PInitialPosition
825 EndIf
826 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
827 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
828 If M_20# = MContinue% Then
829     Mov PPlateLGet_2
830     Mov PPlateLGet_1
831 EndIf
832 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_2
833 *CompPlateLGet_5
834 'Wait M_In(11889) = 1        '�˂����{1��~2��M
835 MRtn = fScrewTighenRoboCheck(11889)    '��~��Ԃ���M����
836 If MRtn = 0 Then Mov PInitialPosition
837 If MRtn = 0 Then GoTo *ASSY_ERROR_END
838 '
839 'Wait M_In(11889) = 1        '�˂����{1��~2��M
840 MRtn = fScrewTighenRoboCheck(11889)    '��~��Ԃ���M����
841 If MRtn = 0 Then Mov PInitialPosition
842 If MRtn = 0 Then GoTo *ASSY_ERROR_END
843 '
844 Mov PPlateLSet_1            '����L�u�����
845 Ovrd 10
846 Mvs PPlateLSet              '����L��u���ʒu
847 Dly 0.2
848 M_Out(12866) = 1 Dly 0.5    '�˂����{1����ĊJ(��~2�`��~3)
849 'Wait M_In(11890) = 1        '�˂����{1��~3��M
850 MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����
851 'If MRtn = 0 Then
852 '    Mvs PPlateLSet_1
853 '    Mov PPlateLSet_2
854 '    Mov PInitialPosition
855 'EndIf
856 If MRtn = 0 Then GoTo *ASSY_ERROR_END
857 M_Out(12262) = 0            '���`���b�N��OFF
858 M_Out(12263) = 1            '���`���b�N�JON
859 Dly 0.5
860 Mvs PPlateLSet_1            '����L�u�����
861 Ovrd 100
862 M_Out(12258) = 0            '����L�V�����_�[�oOFF
863 M_Out(12259) = 1            '����L�V�����_�[��ON
864 Mov PPlateLSet_2            '����L�u�����_
865 '
866 '    ' ���i�����v�����M'12/20�ʒu�ύX(����)
867     M_Out(12787) = 1
868 '
869 '����L�u���ʒu�摜����
870 Mov PPlateLCheck_2          '����L�����ʉߓ_
871 Mvs PPlateLCheck            '����L�����ʒu
872 *RE_L_CHECK
873 PInspPosition(1) = PPlateLCheck
874 MInspGroup%(1) = 2
875 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,1,-1,1)
876 If MRtn = 1 Or M_In(11374) = 0 Then GoTo *CompLCheck
877 fErrorProcess(11,43,3,0)
878 If M_20# = MNext% Then M_20# = MClear%
879 If M_20# = MAbout% Or M_20# = MNgProcess% Then
880     Mov PInitialPosition
881     Break
882 EndIf
883 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
884 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
885 If M_20# = MContinue% Then GoTo *RE_L_CHECK
886 *CompLCheck
887 M_Out(12866) = 1 Dly 0.5    '�˂����{����ĊJ(��~3�`��~4)
888 '
889 '����R�������@������
890 '
891 '    ' ���i�����v�����M'12/20�ʒu�ύX(����)
892 '    M_Out(12787) = 1
893 '    '    ' ���i���������҂�
894 '    Wait M_In(11810) = 1
895 '
896 Mov PPlateRGet_4            '�o�H1
897 Mov PPlateRGet_3            '�o�H2
898 Mov PPlateRGet_2            '����R�����_
899     '    ' ���i���������҂�(�����ύX2/27����)
900 *RE_FEEDER_READY
901     fnAutoScreenComment(513)    '��ԕ\��[���i�����҂�] 2022/04/27 �n��
902 '    Wait M_In(11810) = 1
903 MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '�����҂�
904 If MRtn = 1 Then GoTo *CompFeederReady
905 '   ' ���i�����v���I��
906 M_Out(12787) = 0
907 fErrorProcess(11,289,290,0) '284��290�ɕύX6/7����
908 If M_20# = MNext% Then M_20# = MClear%
909 If M_20# = MAbout% Or M_20# = MNgProcess% Then
910     Mov PBracketRGet_2
911     Mov PBracketRGet_3
912     Mov PBracketRSet_3
913     Mov PInitialPosition1
914 EndIf
915 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
916 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
917     ' ���i�����v��
918 M_Out(12787) = 1
919 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
920 *CompFeederReady
921 '    ' ���i�����v���I��
922     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/27 �n��
923     M_Out(12787) = 0
924 '
925 *RE_PLATE_R_GET_1
926 '
927 If M_20# = MContinue% Then M_20# = MClear%
928 '
929 M_Out(12257) = 0            '�{�̃`���b�N�JOFF(�ȉ�3�s,���󂯎�莞�ז��ɂȂ邽��)
930 M_Out(12256) = 1            '�{�̃`���b�N��ON
931 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '�{�̃`���b�N�Z���T�[ON
932 Dly 0.1
933 If MRtn = 1 Then GoTo *CompPlateRGet_1
934 fErrorProcess(11,245,284,0)
935 If M_20# = MNext% Then M_20# = MClear%
936 If M_20# = MAbout% Or M_20# = MNgProcess% Then
937     Mov PPlateRGet_2
938     Mov PPlateRGet_3
939     Mov PPlateRGet_4
940     Mov PInitialPosition
941     Break
942 EndIf
943 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
944 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
945 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_1
946 *CompPlateRGet_1
947 '
948 *RE_PLATE_R_GET_2
949 If M_20# = MContinue% Then M_20# = MClear%
950 '
951 Mov PPlateRGet_1            '����R�����
952 Ovrd 10
953 M_Out(12261) = 0            '����R�V�����_�[��OFF
954 M_Out(12260) = 1            '����R�V�����_�[�oON
955 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)        '����R�V�����_�[�o�[���o�Z���T�[ON
956 If MRtn = 1 Then GoTo *CompPlateRGet_2
957 fErrorProcess(11,248,284,0)
958 If M_20# = MNext% Then M_20# = MClear%
959 If M_20# = MAbout% Or M_20# = MNgProcess% Then
960     Mov PPlateRGet_2
961     Mov PPlateRGet_3
962     Mov PPlateRGet_4
963     Mov PInitialPosition
964     Break
965 EndIf
966 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
967 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
968 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_2
969 *CompPlateRGet_2
970 '
971 M_Out(12262) = 0            '���`���b�N��OFF
972 M_Out(12263) = 1            '���`���b�N�JON
973 Fine 0.05 , P               '����0.05[mm]�ȓ�
974 Mvs PPlateRGet              '����R�����ʒu
975 Fine 0 , P                  'Fine����
976 M_Out(12263) = 0            '���`���b�N�JOFF
977 M_Out(12262) = 1            '���`���b�N��ON
978 Dly 0.5
979 Mvs PPlateRGet_1            '����R�����
980 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '����R���o
981 If MRtn = 1 Then GoTo *CompPlateRGet_3
982 fErrorProcess(11,254,292,0) '284��292�ɕύX(6/7����)
983 If M_20# = MNext% Then M_20# = MClear%
984 If M_20# = MAbout% Or M_20# = MNgProcess% Then
985     Mov PPlateRGet_2
986     Mov PPlateRGet_3
987     Mov PPlateRGet_4
988     Mov PInitialPosition
989     Break
990 EndIf
991 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
992 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
993 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_2
994 *CompPlateRGet_3
995 '
996 M_Out(12256) = 0            '�{�̃`���b�N��OFF(���󂯎�莞�ז��ɂȂ邽��)
997 M_Out(12257) = 1            '�{�̃`���b�N�JON(���󂯎�莞�ז��ɂȂ邽��)
998 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)        '�{�̃`���b�N�J�Z���T�[ON
999 If MRtn = 1 Then GoTo *CompPlateRGet_4
1000 fErrorProcess(11,244,284,0)
1001 If M_20# = MNext% Then M_20# = MClear%
1002 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1003     Mov PPlateRGet_2
1004     Mov PPlateRGet_3
1005     Mov PPlateRGet_4
1006     Mov PInitialPosition
1007     Break
1008 EndIf
1009 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1010 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1011 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_1
1012 *CompPlateRGet_4
1013 '
1014 Mov PPlateRGet_2            '����R�����_
1015 Ovrd 100
1016 Mov PPlateRGet_3            '�o�H2
1017 ''    ' ���i�����v���I��
1018 '    M_Out(12787) = 0
1019 ''    ' ���i�擾�������M(�p���X)
1020 '    M_Out(12800) = 1 Dly 0.5
1021     '
1022 Mov PPlateRGet_4            '�o�H1
1023 '
1024 '����R��u��
1025 Mov PPlateRSet_3            '�o�H
1026 Mov PPlateRSet_2            '����R�u�����_
1027 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '������x����R���o
1028 If MRtn = 1 Then GoTo *CompRGet
1029 fErrorProcess(11,254,292,0) '284��292�ɕύX(6/7����)
1030 If M_20# = MNext% Then M_20# = MClear%
1031 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1032     Mov PPlateRSet_3
1033     Mov PPlateRGet_3
1034 '    Mov PPlateRGet_4
1035     Mov PInitialPosition
1036     Break
1037 EndIf
1038 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1039 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1040 If M_20# = MContinue% Then
1041     Mov PPlateRSet_3
1042     Mov PPlateRGet_4
1043     Mov PPlateRGet_3
1044     Mov PPlateRGet_2
1045     Mov PPlateRGet_1
1046 EndIf
1047 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_1
1048 *CompRGet
1049 '
1050 '    ' ���i�����v���I��
1051     M_Out(12787) = 0
1052 '    ' ���i�擾�������M(�p���X)
1053     M_Out(12800) = 1 Dly 0.5
1054 'Wait M_In(11891) = 1        '�˂����{1��~4��M
1055 MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
1056 If MRtn = 0 Then Mov PInitialPosition
1057 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1058 Mov PPlateRSet_1            '����R�u�����
1059 Ovrd 10
1060 Mvs PPlateRSet              '����R��u���ʒu
1061 Dly 0.2
1062 M_Out(12866) = 1 Dly 0.5    '�˂����{����ĊJ(��~4�`��~5)
1063 'Wait M_In(11892) = 1        '�˂����{1��~5��M
1064 MRtn = fScrewTighenRoboCheck(11892)    '��~��Ԃ���M����
1065 'If MRtn = 0 Then
1066 '    Mvs PPlateRSet_1
1067 '    Mov PPlateRSet_2
1068 '    Mov PInitialPosition
1069 'EndIf
1070 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1071 M_Out(12262) = 0            '���`���b�N��OFF
1072 M_Out(12263) = 1            '���`���b�N�JON
1073 Dly 0.5
1074 Mvs PPlateRSet_1            '����L�u�����
1075 Ovrd 100
1076 M_Out(12260) = 0            '����R�V�����_�[�oOFF
1077 M_Out(12261) = 1            '����R�V�����_�[��ON
1078 Mov PPlateRSet_2            '����R�u�����_
1079 '
1080 '����R�u���ʒu�摜����
1081 Mov PPlateRCheck_2          '����R�����ʉߓ_
1082 Mvs PPlateRCheck            '����R�����ʒu
1083 *RE_R_CHECK
1084 If M_20# = MContinue% Then M_20# = MClear%
1085 PInspPosition(1) = PPlateRCheck
1086 MInspGroup%(1) = 3
1087 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,1,-1,1)
1088 If MRtn = 1 Or M_In(11374) = 0 Then GoTo *CompCheckR
1089 fErrorProcess(11,43,3,0)
1090 If M_20# = MNext% Then M_20# = MClear%
1091 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1092     Mvs PPlateRCheck_2
1093     Mov PInitialPosition
1094 EndIf
1095 If M_20# = MAbout% Or M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1096 If M_20# = MContinue% Then GoTo *RE_R_CHECK
1097 *CompCheckR
1098 M_Out(12866) = 1 Dly 0.5    '�˂����{����ĊJ(��~5�`��~6)
1099 '
1100 '�˂����{1�̐��i�����
1101 Mov PProductOnRoboGet_2     '�˂����{1���_
1102 'Wait M_In(11893) = 1        '�˂����{1��~6��M
1103 'MRtn = frInCheck(11876,1,MSETTIMEOUT05&)   '�˂����{1��������M
1104 '
1105 *RE_CYLINDER_R_INI
1106 MRtn = frInCheck(11273,1,MSETTIMEOUT05&)        'R���V�����_�[�ߌ��o
1107 If MRtn = 1 Then GoTo *CompCylinderRIni
1108 fErrorProcess(11,249,284,0)
1109 If M_20# = MNext% Then M_20# = MClear%
1110 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1111     Mov PPlateRSet_3
1112     Mov PPlateRGet_3
1113 '    Mov PPlateRGet_4
1114     Mov PInitialPosition
1115     Break
1116 EndIf
1117 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1118 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1119 If M_20# = MContinue% Then
1120     M_Out(12260) = 0            '����R�V�����_�[�oOFF
1121     M_Out(12261) = 1            '����R�V�����_�[��ON
1122 EndIf
1123 If M_20# = MContinue% Then GoTo *RE_CYLINDER_R_INI
1124 *CompCylinderRIni
1125 '
1126 MRtn = fScrewTighenRoboCheck(11893)    '��~��Ԃ���M����
1127 If MRtn = 0 Then Mov PInitialPosition
1128 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1129 'Mvs PProductOnRoboGet_1     '�˂����{1���(2022/1/11�ړ��^�C�~���O�ύX(����))
1130 'Ovrd 25
1131 '
1132 *RE_ROBO_GET
1133 '
1134 If M_20# = MContinue% Then M_20# = MClear%
1135 '
1136 M_Out(12256) = 0            '�{�̃`���b�N��OFF
1137 M_Out(12257) = 1            '�{�̃`���b�N�JON
1138 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)        '�{�̃`���b�N�J�Z���T�[ON
1139 If MRtn = 1 Then GoTo *CompRoboGet_1
1140 fErrorProcess(11,244,284,0)
1141 If M_20# = MNext% Then M_20# = MClear%
1142 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1143     Mvs PProductOnRoboGet_2
1144     Mov PInitialPosition
1145     Break
1146 EndIf
1147 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1148 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1149 If M_20# = MContinue% Then GoTo *RE_ROBO_GET
1150 *CompRoboGet_1
1151 '
1152 Mvs PProductOnRoboGet_1     '�˂����{1���(2022/1/11�ړ��^�C�~���O�ύX(����))
1153 Ovrd 25
1154 '
1155 Mvs PProductOnRoboGet       '�{�̂����ʒu
1156 M_Out(12866) = 1 Dly 0.5    '�˂����{����ĊJ(��~6�`����)
1157 'Wait M_In(11876) = 1        '�˂����{1��������M
1158 MRtn = fScrewTighenRoboCheck(11876)    '��~��Ԃ���M����
1159 If MRtn = 0 Then
1160     Mvs PProductOnRoboGet_1
1161     Mvs PProductOnRoboGet_2
1162     Mov PInitialPosition
1163 EndIf
1164 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1165 '
1166 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1167 M_Out(12256) = 1            '�{�̃`���b�N��ON
1168 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '�{�̃`���b�N�Z���T�[ON
1169 If MRtn = 1 Then GoTo *CompRoboGet_2
1170 M_Out(12256) = 0            '�{�̃`���b�N��OFF
1171 M_Out(12257) = 1            '�{�̃`���b�N�JON
1172 Dly 2.0
1173 Mvs PProductOnRoboGet_1
1174 Mvs PProductOnRoboGet_2
1175 Mov PInitialPosition
1176 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1177 M_Out(12256) = 1            '�{�̃`���b�N��ON
1178 fErrorProcess(11,245,284,0)
1179 If M_20# = MNext% Then M_20# = MClear%
1180 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1181 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1182 M_Out(12256) = 0            '�{�̃`���b�N��OFF
1183 M_Out(12257) = 1            '�{�̃`���b�N�JON
1184 Dly 2.0
1185 Mov PProductOnRoboGet_2
1186 If M_20# = MContinue% Then GoTo *RE_ROBO_GET
1187 Mvs PProductOnRoboGet_1
1188 Mvs PProductOnRoboGet
1189 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1190 M_Out(12256) = 1            '�{�̃`���b�N��ON
1191 Dly 2.0
1192 *CompRoboGet_2
1193 '
1194 MRtn = frInCheck(11264,1,MSETTIMEOUT05&)        '�{�̌��o�Z���T�[ON
1195 'Mvs PProductOnRoboGet_1     '�˂����{1���
1196 If MRtn = 1 Then GoTo *CompRoboGet_3
1197 M_Out(12256) = 0            '�{�̃`���b�N��OFF
1198 M_Out(12257) = 1            '�{�̃`���b�N�JON
1199 Dly 2.0
1200 Mvs PProductOnRoboGet_1
1201 Mvs PProductOnRoboGet_2
1202 Mov PInitialPosition
1203 fErrorProcess(11,252,284,0)
1204 If M_20# = MNext% Then M_20# = MClear%
1205 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1206 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1207 Mov PProductOnRoboGet_2
1208 If M_20# = MContinue% Then GoTo *RE_ROBO_GET
1209 Mvs PProductOnRoboGet_1
1210 Mvs PProductOnRoboGet
1211 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1212 M_Out(12256) = 1            '�{�̃`���b�N��ON
1213 Dly 2.0
1214 *CompRoboGet_3
1215 '
1216 Moverride = 2000 / M_OPovrd
1217 If Moverride >100 Then Moverride = 100
1218 Ovrd Moverride
1219 Mov PProductOnRoboGet_2     '�˂����{1���_
1220 '
1221 *ProductOnPltSet
1222 '�p���b�g�ɐ��i��u��
1223 Mov PProductOnPltSet_2     '�p���b�g���_
1224 Mov PProductOnPltSet_1     '�p���b�g���
1225 Ovrd 10
1226 Mvs PProductOnPltSet       '�{�̒u���ʒu
1227 Dly 0.2
1228 '
1229 *RE_PLT_SET
1230 '
1231 If MScrewRoboNgFlg% = 1 And M_20# = MContinue% Then M_20# = MRtn
1232 If M_20# = MContinue% Then M_20# = MClear%
1233 '
1234 M_Out(12256) = 0            '�{�̃`���b�N��OFF
1235 M_Out(12257) = 1            '�{�̃`���b�N�JON
1236 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    '�{�̃`���b�N�J�Z���T�[ON
1237 If MRtn = 1 Then GoTo *CompPltSet_1
1238 If MScrewRoboNgFlg% = 1 Then
1239     MRtn = M_20#
1240     M_20# = MClear%
1241 EndIf
1242 fErrorProcess(11,244,284,0)
1243 If M_20# = MNext% Then M_20# = MClear%
1244 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1245 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1246 If M_20# = MContinue% Then GoTo *RE_PLT_SET
1247 *CompPltSet_1
1248 If MScrewRoboNgFlg% = 1 Then M_20# = MRtn
1249 '
1250 Mvs PProductOnPltSet_1     '�p���b�g���
1251 Ovrd 100
1252 Mvs PProductOnPltSet_2     '�p���b�g���_
1253 '
1254 'Mov PInitialPosition        '�b��폜(11/17����)
1255     MRtn = FnCtlValue2(2)          '�g���n�j�{�P  2022/04/28 �n��
1256 Mov PTicketRead_1
1257     MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1258 If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
1259 M_Out(12868) = 1 Dly 0.5    '�˂����{1�˂����ߊ����𑗐M
1260 '
1261 M_20# = MAssyOK%            'Assy����I��
1262 '
1263 GoTo *AssyEnd
1264 '
1265 *ASSY_ERROR_END
1266     M_Out(12264) = 0            '�ʒu���ߏoOFF
1267     M_Out(12265) = 1            '�ʒu���ߖ�ON
1268 *AssyEnd
1269 *fnAssyStart_FEndPosi
1270     Exit Function
1271 FEnd
1272 '
1273 '��fnPiasCheck
1274 ''' <summary>
1275 ''' PIAS�`�P�b�g�Ǎ���
1276 ''' </summary>
1277 ''' <returns>   0 : NG
1278 '''             1 : OK(�Ǎ��݊���)
1279 ''' </returns>
1280 ''' <remarks>
1281 ''' Date   : 2021/07/07 : M.Hayakawa
1282 ''' </remarks>'
1283 Function M% fnPiasCheck
1284     fnPiasCheck = 0
1285     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1286     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1287 '
1288 *RETRY_PIAS
1289     M_20# = MClear%
1290     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1291     '
1292     '�yID�`�P�b�g�ǂݍ��݁z
1293     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1294     MInspGroup%(1) = 1              '����G�ԍ�
1295     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1296 '
1297     '�G���[�̏ꍇ
1298     If MRtn <> 1 Then
1299         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1300         If MRtn <> 1 Then
1301             'D720 -> D1300 �R�s�[�v��
1302             M_Out(12565) = 1
1303             Dly 0.5
1304             M_Out(12565) = 0
1305             '�G���[�����L�q
1306             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1307             'GOT KEY���͑҂�
1308             MKeyNumber = fnKEY_WAIT()
1309             '
1310             Select MKeyNumber
1311                 Case MNext%         '���ւ�I�������ꍇ
1312                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1313                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1314                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1315                     Break
1316                 Case MAbout%        '��~��I�������ꍇ
1317                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1318                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1319                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1320                     Break
1321                 Case MNgProcess%    'NG��I�������ꍇ
1322                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1323                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1324                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1325                     Break
1326                 Case MContinue%     '�p����I�������ꍇ
1327                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1328                     M_20# = MContinue%
1329                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1330                     Break
1331             End Select
1332         EndIf
1333     EndIf
1334 '----------D720 -> D1300 �R�s�[�v��----------
1335     M_Out(12565) = 1
1336     Dly 0.5
1337     M_Out(12565) = 0
1338 '----------�ʐM�m�F������----------
1339     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1340     MRtn = 0                ' ������
1341     M_20# = MClear%         ' ������
1342     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1343     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1344     If MRtn <> 1 Then
1345         If M_20# = MContinue% Then
1346             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1347         Else
1348             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1349         EndIf
1350     EndIf
1351 '----------�H�������m�F----------
1352     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1353     MRtn = 0                ' ������
1354     M_20# = MClear%         ' ������
1355     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1356     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1357     If MRtn <> 1 Then
1358         If M_20# = MContinue% Then
1359             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1360         Else
1361             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1362         EndIf
1363     EndIf
1364     '
1365     fnPiasCheck = 1
1366     *fnPiasCheck_End
1367     Exit Function
1368 FEnd
1369 '
1370 '��fnPCComuCheck
1371 ''' <summary>
1372 ''' PC-PLC�ʐM�`�F�b�N
1373 ''' </summary>
1374 ''' <returns>   0 : NG
1375 '''             1 : OK(�Ǎ��݊���)
1376 ''' </returns>
1377 ''' <remarks>
1378 ''' Date   : 2021/07/07 : M.Hayakawa
1379 ''' </remarks>'
1380 Function M% fnPCComuCheck
1381     fnPCComuCheck = 0
1382     MJudge% = 0                                  '������
1383     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1384     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1385     '
1386     For MStaNo = 0 To 5
1387         '
1388         If M_In(MIN_PIAS_ComOK%) = 1 Then
1389             'PC�ʐMOK(M400)
1390             MJudge% = MOK%
1391             MStaNo = 5
1392             Break
1393         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1394             'toRBT_�ʐM�m�Ftime out
1395             MJudge% = MNG%
1396             MCommentD1001 = 15
1397             MCommentD1002 = 21
1398             MStaNo = 5
1399             Break
1400         Else
1401             'toRBT_�ʐM�m�Ftime out
1402             MJudge% = MNG%
1403             MCommentD1001 = 14
1404             MCommentD1002 = 21
1405             Break
1406         EndIf
1407     Next MStaNo
1408     '
1409     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1410     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1411     '
1412     '�G���[���
1413     If MJudge% <> MOK% Then
1414         M_20# = MClear%     '������
1415         '�G���[�����L�q
1416         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1417         'GOT KEY���͑҂�
1418         MKeyNumber = fnKEY_WAIT()
1419         '
1420         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1421             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1422             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1423             Break
1424         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1425             M_20# = MPass%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1426             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1427             Break
1428         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1429             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1430             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1431             Break
1432         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1433             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1434             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1435             Break
1436         EndIf
1437     Else
1438         'OK�̏ꍇ
1439         fnPCComuCheck = 1
1440     EndIf
1441     Exit Function
1442 FEnd
1443 '
1444 '��fnProcessCheck
1445 ''' <summary>
1446 ''' �H�������m�F
1447 ''' </summary>
1448 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1449 '''             -1�F�O�H������NG  -2�F���H����������
1450 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1451 '''             -5�F���������G���[
1452 ''' </returns>
1453 ''' <remarks>
1454 ''' Date   : 2021/07/07 : M.Hayakawa
1455 ''' </remarks>'
1456 Function M% fnProcessCheck
1457     fnProcessCheck = 0
1458     MJudge% = MNG%      '��UNG���������Ƃ���
1459 '----------�H�������m�F----------
1460     MCommentD1001 = 0   '�R�����g������
1461     For MStaNo = 0 To 5
1462         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1463         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1464         '
1465         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1466             MJudge% = MOK%
1467             fnAutoScreenComment(85)     ' AUTO���
1468             MStaNo = 5
1469             Break
1470         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1471             MFlgLoop% = 0
1472             MJudge% = MNG%
1473             MCommentD1001 = 27
1474             MCommentD1002 = 22
1475             fnAutoScreenComment(94)     ' AUTO���
1476             fnProcessCheck = -2         ' NG��-2��Ԃ�
1477             MStaNo = 5
1478             Break
1479         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1480            MJudge% = MNG%
1481             MCommentD1001 = 31
1482             MCommentD1002 = 22
1483             fnAutoScreenComment(83)     ' AUTO���
1484             fnProcessCheck = -3         ' NG��-3��Ԃ�
1485             MStaNo = 5
1486             Break
1487         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1488             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1489             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1490             MJudge% = MNG%
1491             MCommentD1001 = 32
1492             MCommentD1002 = 22
1493             fnAutoScreenComment(84)     ' AUTO���
1494             fnProcessCheck = -1         ' NG��-1��Ԃ�
1495             Dly 1.0
1496             '�H�������m�FOFF
1497             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1498             Dly 1.0
1499            'MStaNo = 5
1500             Break
1501         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1502             MFlgLoop% = 0
1503             MJudge% = MNG%
1504             MCommentD1001 = 29
1505             MCommentD1002 = 22
1506             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1507             fnProcessCheck = -5         ' NG��-5��Ԃ�
1508             MStaNo = 5
1509             Break
1510         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1511             MJudge% = MNG%
1512             If MCommentD1001 = 32 Then
1513                 '�������Ȃ�
1514             Else
1515                 MCommentD1001 = 26
1516             EndIf
1517             MCommentD1002 = 22
1518             fnProcessCheck = -4         ' NG��-4��Ԃ�
1519             MStaNo = 5
1520             Break
1521         Else
1522             MJudge% = MNG%
1523             MCommentD1001 = 28
1524             MCommentD1002 = 22
1525         EndIf
1526     Next MStaNo
1527     '�H�������m�FOFF
1528     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1529     '�ʉߗ���NG �H�������̏ꍇ
1530     If MJudge% = MPass% Then
1531         M_20# = MPass%
1532     EndIf
1533     '
1534     '�G���[���
1535     If MJudge% <> MOK% Then
1536         M_20# = MClear%     '������
1537         '�G���[�����L�q
1538         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1539         'GOT KEY���͑҂�
1540         MKeyNumber = fnKEY_WAIT()
1541         '
1542         Select MKeyNumber
1543             Case MAbout%        '��~��I�������ꍇ
1544                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1545                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1546                 Break
1547             Case MNext%         '���ւ�I�������ꍇ
1548                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1549                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1550                 Break
1551             Case MContinue%     '�p����I�������ꍇ
1552                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1553                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1554                 Break
1555             Case MNgProcess%    'NG��I�������ꍇ
1556                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1557                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1558                 Break
1559         End Select
1560     Else
1561         fnProcessCheck = 1  ' OK��1��Ԃ�
1562     EndIf
1563     Exit Function
1564 FEnd
1565 '
1566 '��fnPiasWrite
1567 ''' <summary>
1568 ''' Pias �g�����ʏ����ݗv��
1569 ''' </summary>
1570 '''<param name="MFlg%">
1571 '''                 MOK%(1) = �H��������OK��������
1572 '''                 MNG%(0) = �H��������NG��������
1573 '''</param>
1574 '''<returns></returns>
1575 ''' <remarks>
1576 ''' Date   : 2021/07/07 : M.Hayakawa
1577 ''' </remarks>'
1578 Function M% fnPiasWrite(ByVal MFlg%)
1579       fnPiasWrite = 0
1580 *RETRY_PIASWRITE
1581     '
1582     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1583    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1584     If MFlg% = MOK% Then
1585         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1586     Else
1587         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1588     EndIf
1589     Dly 0.1                  '�O�̂���
1590     '
1591     'Pias�֏����݊J�n M305 -> ON
1592     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1593     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1594     '
1595     MJudge% = MNG%
1596     '
1597     For MStaNo = 0 To 5
1598         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1599             MJudge% = MOK%
1600             'MRet = fnAutoScreenComment(85)  'AUTO���
1601             MStaNo = 5
1602             Break
1603         '
1604         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1605             MJudge% = MNG%
1606             'MRet = fnAutoScreenComment(85)  'AUTO���
1607            MCommentD1001 = 34
1608            MCommentD1002 = 25
1609             MStaNo = 5
1610             Break
1611         '
1612         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1613             MJudge% = MNG%
1614             'MRet = fnAutoScreenComment(85)  'AUTO���
1615            MCommentD1001 = 35
1616            MCommentD1002 = 25
1617             MStaNo = 5
1618             Break
1619         '
1620         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1621             MJudge% = MNG%
1622             'MRet = fnAutoScreenComment(85)  'AUTO���
1623            MCommentD1001 = 36
1624            MCommentD1002 = 25
1625             MStaNo = 5
1626             Break
1627         '
1628         Else
1629             MJudge% = MNG%
1630            MCommentD1001 = 42
1631            MCommentD1002 = 25
1632         '
1633         EndIf
1634         '
1635     Next MStaNo
1636     '
1637     'Pias�֏����݊J�n M305 -> OfF
1638     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1639     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1640     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1641     '
1642     '
1643     '�ʉߗ���NG �H�������̏ꍇ
1644     If MJudge% = MPass% Then
1645         M_20# = MPass%
1646     EndIf
1647     '
1648    M_20# = MClear%     '������
1649     '
1650     '�G���[���
1651     If MJudge% < MOK% Then
1652     '
1653 '�c���Ă���������ł͎g�p���Ȃ����x��
1654 *RETRY_ERR_WRITE
1655         M_20# = MClear%     '������
1656         '�G���[�����L�q
1657         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1658         'GOT KEY���͑҂�
1659         MKeyNumber = fnKEY_WAIT()
1660         '
1661         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1662             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1663            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1664             Break
1665         '
1666         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1667             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1668             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1669         '
1670         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1671             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1672             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1673         '
1674         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1675             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1676            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1677             Break
1678         '
1679         EndIf
1680         '
1681         If M_20# = MClear% Then *RETRY_ERR_WRITE
1682         '
1683     EndIf
1684     '
1685     If M_20# = MContinue% Then *RETRY_PIASWRITE
1686     '
1687     fnPiasWrite = 1
1688     Exit Function
1689 FEnd
1690 '
1691 '��fnPCBNumberCheck
1692 ''' <summary>
1693 ''' Pias ��ԍ��ƍ��v��
1694 ''' </summary>
1695 '''<param name="%"></param>
1696 '''<param name="%"></param>
1697 '''<returns></returns>
1698 ''' <remarks>
1699 ''' Date   : 2021/07/07 : M.Hayakawa
1700 ''' </remarks>'
1701 Function M% fnPCBNumberCheck
1702       fnPCBNumberCheck = 0
1703     '
1704 *RETRY_PCBCHECK
1705     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1706     'Pias�֊�ƍ��J�n M310 -> ON
1707     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1708     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1709     '
1710     MJudge% = MNG%
1711     '
1712     For MStaNo = 0 To 5
1713         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1714             MJudge% = MOK%
1715             fnAutoScreenComment(96)  'AUTO���
1716             MStaNo = 5
1717             Break
1718         '
1719         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1720             MJudge% = MNG%
1721             fnAutoScreenComment(97)  'AUTO���
1722             MCommentD1001 = 37
1723             MCommentD1002 = 25
1724             MStaNo = 5
1725             Break
1726         '
1727         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1728             MJudge% = MNG%
1729             fnAutoScreenComment(98)  'AUTO���
1730             MCommentD1001 = 38
1731             MCommentD1002 = 25
1732             MStaNo = 5
1733             Break
1734         '
1735         ElseIf M_In(11580) = 1 Then                         'time out
1736             MJudge% = MNG%
1737             fnAutoScreenComment(99)  'AUTO���
1738             MCommentD1001 = 39
1739             MCommentD1002 = 25
1740             MStaNo = 5
1741             Break
1742         '
1743         Else
1744             MJudge% = MNG%
1745            MCommentD1001 = 41
1746            MCommentD1002 = 25
1747         '
1748         EndIf
1749         '
1750     Next MStaNo
1751     '
1752     'Pias�֊�ƍ��J�n M310 -> OfF
1753     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1754     '
1755     '
1756     '�ʉߗ���NG �H�������̏ꍇ
1757     If MJudge% = MPass% Then
1758         M_20# = MPass%
1759     EndIf
1760     '
1761    M_20# = MClear%     '������
1762     '
1763     '�G���[���
1764     If MJudge% < MOK% Then
1765     '
1766 '�c���Ă���������ł͎g�p���Ȃ����x��
1767 *RETRY_ERR_PCBNUMBER
1768         M_20# = MClear%     '������
1769         '�G���[�����L�q
1770         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1771         'GOT KEY���͑҂�
1772         MKeyNumber = fnKEY_WAIT()
1773         '
1774         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1775             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1776             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1777             Break
1778         '
1779         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1780             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1781             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1782         '
1783         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1784             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�'MPass%��MNext%�֕ύX(12/7����)
1785             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1786         '
1787         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1788             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1789             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1790             Break
1791         '
1792         EndIf
1793         '
1794         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1795         '
1796     EndIf
1797     '
1798     If M_20# = MContinue% Then *RETRY_PCBCHECK
1799     Exit Function
1800 FEnd
1801 '
1802 '��ScrewTight_S2
1803 ''' <summary>
1804 ''' �˂����߂��s��
1805 ''' </summary>
1806 '''<param name="PScrewPos()">
1807 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1808 '''             PScrewPos(2)    �F�˂����߉��_
1809 '''             PScrewPos(10)   �F�˂����ߏI������
1810 '''</param>
1811 '''<returns>����
1812 '''         0=�ُ�I���A1=����I��
1813 '''</returns>
1814 ''' <remarks>
1815 ''' Date   : 2021/07/07 : M.Hayakawa
1816 ''' </remarks>'
1817 Function M% ScrewTight_S2(ByVal PScrewPosition())   '�l�W���ߌʐݒ�
1818     ScrewTight_S2 = 0
1819     MOKNGFlg = 0
1820     Ovrd 100
1821     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1822     ' �b��
1823     Ovrd 5
1824     Mvs PScrewPosition(10),-10    ' �p���b�g��˂�����S�@�̏��ֈړ�
1825 '    Ovrd MOvrdA
1826     '�b��}�X�N
1827 '    M_Out(Y62_Driver)=1     ' �o���N�Z�b�e�B���O�@C1
1828 '    Dly 0.1
1829 '    M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
1830 '    'Spd 8.3 '�O�����C�h100  �������C�h60   '���C�h100-40�@100%�FSpd�@15�@'�˂����ߑ��x�ݒ�
1831 '    Spd MSpdA               '�l�W���ߎ�Spd�ʐݒ�
1832     ' �b��ړ��̂�
1833     Mvs PScrewPosition(10)
1834 '    '
1835 '    Dly 0.1
1836 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1837 '    Wait M_In(11584)=1          '����/�G���[���o
1838 '    Dly 0.1
1839 '    Spd M_NSpd
1840 '    '
1841 '    If M_In(X28_Driver)=1 Then  '�˂��g�[�^���G���[���o��
1842 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1843 '        Dly 0.1
1844 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1845 '        Dly 0.1
1846 '        M_Out(Y63_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1847 '        Dly 0.1
1848 '        M_Out(Y65_Driver)=0     '�v���O���������@F1
1849 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1850 '        M_Out(Y68_VV1)=0        '�˂��z���@OFF
1851 '        MOKNGFlg = -1
1852 '        ScrewTight_S2 = 0
1853 '    Else
1854 '        Wait M_In(X29_Driver)=1 ' ���튮����
1855 '        Dly 0.1
1856 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1857 '        Dly 0.1
1858 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O����
1859 '        Dly 0.1
1860         M_Out(12249)=1 Dly 0.3         '�˂��z���@OFF (�ꎞ�R�����g�A�E�g����,(Y68_VV1)=0��(12249)=1 Dly 0.3�ɕύX(8/5����))
1861         M_Out(12250)=1 Dly 0.1         '�b��^��j��
1862 '        Dly 0.1
1863 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1864 '        ScrewTight_S2 = 1
1865 '    EndIf
1866 ' �b��
1867     Ovrd 10
1868     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1869     Ovrd 100
1870     Exit Function
1871 FEnd
1872 '
1873 '��ScrewGet_S3
1874 ''' <summary>
1875 ''' �˂������@����˂��𓾂�
1876 ''' </summary>
1877 '''<param name="%"></param>
1878 '''         PScrewPos(1)    �F�˂�������̂˂����
1879 '''         PScrewPos(2)    �F�˂���������_
1880 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1881 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
1882 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
1883 '''<returns>����
1884 '''         0=�ُ�I���A1=����I���A-1=M�l�W�Z���T�[NG�A-2=M�l�W�Z���T�[ON�A-3=�z���G���[
1885 '''</returns>
1886 ''' <remarks>
1887 ''' Date   : 2021/07/07 : M.Hayakawa
1888 ''' </remarks>'
1889 Function M% ScrewGet_S3(ByVal PScrewPosition())
1890     ScrewGet_S3 = 0
1891     MMScrewJudge% = 0
1892     '�˂������평������G���[�`�F�b�N
1893 ' ���b��폜
1894 '    Wait M_In(X34_ScrewReady1)=1 '�˂�������S��Ready�ɂȂ�܂ő҂@���@���b���҂���Ready�ɂȂ�Ȃ���Δ�����v���O�������K�v�H
1895 '    Ovrd 100
1896 '    If M_In(X33_SS2)=0 Then  'M�˂����o�Z���T��OFF�i�̏�j���Ă����ꍇ
1897 '        Ovrd 30
1898 '        Mvs,-80             '���̏ꏊ����80mm���ֈړ�
1899 '        Mov PInitPos19049   '19049�����ʒu�ֈړ�
1900 '        M_Out(Y68_VV1)=0    '�˂��z�� Off
1901 '        'NG�Ƃ��Ă����̊֐����甲����
1902 '        ScrewGet_S3 = -1
1903 '        MMScrewJudge% = 1
1904 '        MCommentD1001 = 61
1905 '    EndIf
1906 '    If ScrewGet_S3 = 0 Then
1907 '        'S�^�C�g�p�˂������@��M�˂����������Ă��Ȃ����Ď�
1908 '        MMScrewJudge% = 0 'MMScrewJudge������������
1909 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
1910 '        If MRtn = 0 Then
1911 '            Ovrd 30
1912 '            Mvs,-80            '���̏ꏊ����50mm���ֈړ�
1913 '            Mov PInitPos19049  '19049�����ʒu�ֈړ�
1914 '            MMScrewJudge% = 2
1915 '            MRtn = All_CLamp_Release()'�S�ẴN�����v�����֕���
1916 '            MCnt% = 2   '2��ݒ�
1917 '            MCommentD1001 = 62
1918 '        EndIf
1919 '        If MMScrewJudge% = 2 Then
1920 '            ScrewGet_S3 = -2
1921 '        EndIf
1922 '    EndIf
1923 '    'M�l�W���肪ON�̏ꍇ NG�Ƃ��Ċ֐��𔲂���
1924 '    If MMScrewJudge% = 2 Then
1925 '        ScrewGet_S3 = -2
1926 '    EndIf
1927     'S�l�W�p�˂����Y��M�l�W�����m�F�p�����܂�
1928     Ovrd 100
1929     Spd M_NSpd
1930     If MMScrewJudge% = 0 Then
1931         ScrewGet_S3 = 0
1932         M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
1933         MScrewCnt% = 0
1934         MFinCnt% = 2
1935 '        For MCnt% = 0 To MFinCnt%
1936             Mov PScrewPosition(2)        ' �˂������@���_
1937             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
1938             Ovrd 80
1939             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1940             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
1941             Mvs PScrewPosition(10), 1.2
1942             M_Out(Y68_VV1)=1 Dly 0.3        ' �˂��z���@ON
1943             '�r�b�g��]
1944             M_Out(Y60_Driver)=1
1945             Dly 0.2
1946             '
1947             Ovrd 100
1948             JOvrd M_NJovrd
1949             Spd M_NSpd
1950             '�l�W�z���m�F�ʒu�ړ�
1951             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
1952             Mvs PScrewPosition(10), -15  ' �l�W�z���m�F�ʒu
1953             '�r�b�g��]��~
1954             'M_Out(Y60_Driver)=0
1955             '
1956             '1�b�ԃl�W�z���m�F
1957 ' �ȉ��b��폜
1958 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
1959 '            'MRtn = 0'�����G���[
1960 '            '�z���G���[�̏ꍇ
1961 '            '�l�W���˂����Y�ɖ߂�
1962 '            If MRtn = 0 Then
1963 '                Ovrd 30
1964 '                '�r�b�g��]��~
1965 '                M_Out(Y60_Driver)=0
1966 '                '�l�W�����@���
1967 '                Mvs PScrewPos(1)
1968 '                '�X�ɏ��
1969 '                Mov PScrewPos(1), -75
1970 '                '�l�W�̂Ĉʒu
1971 '                Mov PScrewFeedS021
1972 '                '�z��OFF
1973 '                M_Out(Y68_VV1)=0 '�˂��z���@OFF
1974 '                Dly 0.2
1975 '                '�j��ON
1976 '                M_Out(Y6B_VB1)=1 '�^��j��ON
1977 '                '�r�b�g��]
1978 '                M_Out(Y61_Driver)=1
1979 '                Dly 0.5
1980 '                '
1981 '                Ovrd 100
1982 '                JOvrd M_NJovrd
1983 '                Spd M_NSpd
1984 '                '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1985 '                Mov PScrewFeedS021, 10
1986 '                Mov PScrewFeedS021
1987 '                Dly 0.1
1988 '                Mov PScrewFeedS021, 10
1989 '                Mov PScrewFeedS021
1990 '                '
1991 '                '�l�W�����҂�
1992 '                '�r�b�g��]��~
1993 '                M_Out(Y61_Driver)=0
1994 '                Dly 0.1
1995 '                '�j��OFF
1996 '                M_Out(Y6B_VB1)=0 '�^��j��OFF
1997 '                '
1998 '                '
1999 '                '�˂��������Ƃ��āA�ړ��X�ɏ��
2000 '                Mov PScrewPos(1), -75
2001 '                Ovrd 100
2002 '                Spd M_NSpd
2003 '                '�l�W�����@���
2004 '                Mvs PScrewPos(1)
2005 '                '
2006 '                ScrewGet_S3 = -3
2007 '                Break
2008 '                '
2009 '            Else
2010 '                MCnt% = MFinCnt%
2011 '                ScrewGet_S3 = 0
2012 '            EndIf
2013 '        Next  MCnt%
2014         '
2015         Ovrd 100
2016         Spd M_NSpd
2017         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2018         M_Out(Y60_Driver)=0     ' �r�b�g��]��~
2019         M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
2020         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2021         '������x�z���m�F
2022 ' �ȉ��b��폜
2023 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2024 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
2025 '            MCommentD1001 = 94
2026 '            MCommentD1002 = 95
2027 '            ScrewGet_S3 = -3
2028 '        EndIf
2029 '        If MRtn = 1 Then      '�z��OK�̏ꍇ
2030 '            ScrewGet_S3 = 1
2031 '        EndIf
2032 '        Break
2033     Else
2034         'M�l�W
2035         If MMScrewJudge% = 2 Then
2036             ScrewGet_S3 = -2
2037         EndIf
2038     EndIf
2039     Exit Function
2040 FEnd
2041 '
2042 '��fnKEY_WAIT()
2043 ''' <summary>
2044 ''' GOT����̃L�[���͑҂�
2045 ''' </summary>
2046 '''<returns>1�F��~    2�F����
2047 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
2048 '''         5�FNG
2049 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
2050 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
2051 '''</returns>
2052 ''' <remarks>
2053 ''' Date   : 2021/07/07 : M.Hayakawa
2054 ''' </remarks>'
2055 Function M% fnKEY_WAIT()
2056     fnKEY_WAIT = 0
2057     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
2058     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
2059     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
2060     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2061     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
2062     Dly 0.2
2063     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
2064     MLocalLoopFlg=1
2065     While MLocalLoopFlg=1
2066         If M_In(11345) = 1 Then         '��~   M5345
2067             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
2068             fnKEY_WAIT = 1
2069             MLocalLoopFlg=-1
2070             Break
2071         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
2072             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
2073             fnKEY_WAIT = 2
2074             MLocalLoopFlg=-1
2075             Break
2076         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
2077             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
2078             fnKEY_WAIT = 3
2079             MLocalLoopFlg=-1
2080             Break
2081         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
2082             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
2083             fnKEY_WAIT = 4
2084             MLocalLoopFlg=-1
2085             Break
2086         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
2087             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
2088             fnKEY_WAIT = 5
2089             MLocalLoopFlg=-1
2090             Break
2091             '
2092         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
2093             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
2094             fnKEY_WAIT = MRobotInit1%
2095             MLocalLoopFlg=-1
2096             Break
2097             '
2098         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
2099             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
2100             fnKEY_WAIT = MRobotInit2%
2101             MLocalLoopFlg=-1
2102             Break
2103             '
2104         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
2105             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
2106             fnKEY_WAIT = MRobotInit3%
2107             MLocalLoopFlg=-1
2108             Break
2109             '
2110         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
2111             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2112             fnKEY_WAIT = MRobotInit4%
2113             MLocalLoopFlg=-1
2114             Break
2115             '
2116         Else
2117         EndIf
2118     WEnd
2119     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2120     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2121     Exit Function
2122 FEnd
2123 '
2124 '�� fnAUTO_CTL
2125 ''' <summary>
2126 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2127 ''' </summary>
2128 ''' <remarks>
2129 ''' Date   : 2021/07/07 : M.Hayakawa
2130 ''' </remarks>
2131 Function M% fnAUTO_CTL
2132     fnAUTO_CTL = 0
2133     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2134     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2135     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2136     '
2137     If M_Svo=0 Then             '�T�[�{ON�m�F
2138         Servo On
2139     EndIf
2140     Wait M_Svo=1
2141     Exit Function
2142 FEnd
2143 '
2144 '�� fnWindScreenOpen
2145 ''' <summary>
2146 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2147 ''' </summary>
2148 '''<param name="%"></param>
2149 '''<param name="%"></param>
2150 '''<param name="%"></param>
2151 '''<param name="%"></param>
2152 ''' <remarks>
2153 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2154 ''' MWindReSet = 0     ��ʔ�\��
2155 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2156 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2157 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2158 ''' Date   : 2021/07/07 : M.Hayakawa
2159 ''' </remarks>
2160 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2161     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2162         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2163     EndIf
2164     '
2165     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2166         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2167     EndIf
2168     '
2169     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2170        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2171     EndIf
2172     '
2173     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2174     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2175     Dly 0.5
2176     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2177     Exit Function
2178 FEnd
2179 '
2180 '��FnCtlValue2
2181 ''' <summary>
2182 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2183 ''' </summary>
2184 ''' <param name="MCtlNo%"></param>
2185 ''' <remarks>
2186 ''' Date : 2022/04/28 �n��
2187 ''' </remarks>
2188 '''
2189 '''  1�F������       �{�P
2190 '''  2�F�g���n�j��   �{�P
2191 '''  3�F�g���m�f��   �{�P (���g�p)
2192 '''  4�F�z���G���[�� �{�P
2193 ''' 99�F�Ǐ��J�n�M�� OFF
2194 '''
2195 Function M% FnCtlValue2(ByVal MCtlNo%)
2196     FnCtlValue2 = 1
2197     Select MCtlNo%
2198         Case 1        '�������{�P
2199             M_Out(12569) = 0             '�����݊J�n�M��OFF
2200             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2201             MInputQty = M_In16(11600)    '��������M
2202             MInputQty = MInputQty + 1    '�������{�P
2203             M_Out16(12592) = MInputQty   '���������M
2204             M_Out(12569) = 1             '�����݊J�n�M��ON
2205             Break
2206             '
2207         Case 2        '�g���n�j���{�P
2208             M_Out(12569) = 0             '�����݊J�n�M��OFF
2209             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2210             MAssyOkQty = M_In16(11616)   '�g��OK����M
2211             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2212             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2213             M_Out(12569) = 1             '�����݊J�n�M��ON
2214             Break
2215             '
2216         Case 4        '�z���G���[���{�P
2217             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2218             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2219             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2220             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2221             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2222             M_Out(12569) = 1                       '�����݊J�n�M��ON
2223             Break
2224             '
2225         Case 99        '�Ǐ��J�n�M��OFF
2226             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2227             M_Out(12569) = 0        '�����݊J�n�M��OFF
2228             Break
2229             '
2230     End Select
2231 FEnd
2232 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2233 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2234 '-------------------------------------------------------------------------------
2235 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2236 '   ����
2237 '       PInspPos()      �F�����ʒu
2238 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2239 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2240 '       MInspCnt%       �F�����ʒu��
2241 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2242 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2243 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2244 '   �߂�l�F����
2245 '       0=�ُ�I���A1=����I��
2246 '
2247 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2248 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2249 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2250 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2251 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2252 '-------------------------------------------------------------------------------
2253     '----- �����ݒ� -----
2254     Cnt 0                                                           '�ړ�����������(�����l=0)
2255     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2256 '    Cnt 1,0.1,0.1
2257     '�ϐ��錾�E������
2258     Def Inte MNum                                                   '�����ԍ�(������1�`)
2259     MNum% = 1                                                       '�����ԍ������l�ݒ�
2260     Def Inte MEndFlg                                                '�����I���t���O
2261     MEndFlg% = 0
2262     '
2263     '����G�ԍ��ݒ�v���E�������s�v��off
2264     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2265     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2266     '�G���[�ԍ��N���A
2267     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2268     M_Out16(MOUT_InspErrNum) = MInspErrNum
2269     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2270     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2271     '
2272     'Insight Ready check?
2273     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2274         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2275         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2276         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2277         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2278         Exit Function
2279     EndIf
2280     '
2281     '�����ʒu���m�F
2282     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2283         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2284         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2285         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2286         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2287         Exit Function
2288     EndIf
2289     '
2290     '
2291     '
2292     '----- ���C������ -----
2293     '�ݒ肳�ꂽ�����ʒu�����̌������s
2294     While( MEndFlg% = 0 )
2295         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2296         MSetGrNumRetryExitFlg = 0
2297         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2298         While( MSetGrNumRetryExitFlg = 0 )
2299         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2300             '
2301             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2302             '
2303             '----- �����O���[�v�ԍ��ݒ� -----
2304             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2305             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2306             '
2307             '�����ʒu�ֈړ��E�ړ������҂�
2308             Mvs PInspPos( MNum% )                                       '�ړ�
2309             Dly 0.05                                                    '�ړ�������Delay
2310             '
2311             '�����O���[�v�ԍ��ݒ�I���m�F
2312             M_Timer(1) = 0
2313             MExitFlg = 0
2314             While( MExitFlg = 0 )
2315                 '����G�ݒ萳��I��?
2316                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2317                     MExitFlg = 1
2318                 '
2319                 '����G�ݒ�ُ�I��?
2320                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2321                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2322                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2323                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2324                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2325                     EndIf
2326                     MExitFlg = 1
2327                 '
2328                 'timeout�`�F�b�N
2329                 ElseIf 1000 < M_Timer(1) Then
2330                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2331                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2332                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2333                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2334                     EndIf
2335                     MExitFlg = 1
2336                 EndIf
2337             WEnd
2338             '
2339             '����G�ԍ��ݒ�v��off
2340             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2341             '
2342             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2343             'NG�Ȃ���Δ�����
2344             If MCurrentStepErr = 0 Then
2345                 MSetGrNumRetryExitFlg = 1
2346             Else
2347                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2348                 If MSetGrNumRetryCnt = 0 Then
2349                     MSetGrNumRetryExitFlg = 1
2350                 Else
2351                     'Retry�ց@���̑O��Delay
2352                     Dly 0.5
2353                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2354                 EndIf
2355             EndIf
2356             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2357             '
2358         WEnd
2359         '
2360         '
2361         '
2362         '----- �������s -----
2363         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2364             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2365                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2366                 MInspRetryExitFlg = 0
2367                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2368                 While( MInspRetryExitFlg = 0 )
2369                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2370                     '
2371                     '���������m�F
2372                     MRetryCnt = MRetryCnt - 1
2373                     M_Timer(1) = 0
2374                     MExitFlg = 0
2375                     While( MExitFlg = 0 )
2376                     '���������҂�
2377                         '����OK�I��?
2378                         If M_In( MIN_IS_InspOK% ) = 1  Then
2379                             MJudgeOKFlg = 1                         '����OK�t���OON
2380                             MExitFlg = 1
2381                         '
2382                         '����NG�I��?
2383                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2384                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2385                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2386                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2387                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2388                                 EndIf
2389                             EndIf
2390                             MExitFlg = 1
2391                         '
2392                         '�����ُ�I��(IS timeout)?
2393                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2394                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2395                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2396                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2397                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2398                                 EndIf
2399                             EndIf
2400                             MExitFlg = 1
2401                         '
2402                         'timeout�`�F�b�N
2403                         ElseIf 3000 < M_Timer(1) Then
2404                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2405                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2406                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2407                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2408                                 EndIf
2409                             EndIf
2410                             MExitFlg = 1
2411                         EndIf
2412                     WEnd
2413                     '
2414                     '�����J�n�v��off
2415                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2416                     '
2417                     'OK�Ȃ甲����
2418                     If MJudgeOKFlg = 1 Then
2419                         MInspRetryExitFlg = 1
2420                     Else
2421                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2422                         If MRetryCnt = 0 Then
2423                             MInspRetryExitFlg = 1
2424                         Else
2425                             'Retry�ց@���̑O��Delay
2426                             Dly 0.3
2427                         EndIf
2428                     EndIf
2429                     '
2430                 WEnd
2431             EndIf
2432         EndIf
2433         '
2434         '
2435         '
2436         MNum% = MNum% + 1                                           '����Step+1
2437         '�����I���m�F�@�����I���t���O�Z�b�g
2438         If (MInspCnt% < MNum% ) Then
2439             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2440         EndIf
2441         'NG���������s������
2442         If MInspErrNum <> 0 Then                                    'NG����?
2443             If MNgContinue% <> 1 Then                               'NG���s?
2444                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2445             EndIf
2446         EndIf
2447     WEnd
2448     '
2449     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2450     If 0 < MZAxis% Then
2451         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2452         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2453         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2454     EndIf
2455     '
2456     '�߂�l�ݒ�
2457     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
2458         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2459     Else
2460         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2461         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2462         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2463     EndIf
2464     '
2465     Fine 0 , P
2466     Exit Function
2467 FEnd
2468 '
2469 '��InitialZone
2470 ''' <summary>
2471 ''' ��������Ō��݈ʒu��ǂݍ���ōŊ��̑Ҕ��ꏊ�Ɉړ�
2472 ''' �ݒ肳�ꂽ�Ҕ��ʒu�t�߂̏ꍇ�͂Ȃɂ����Ȃ��B
2473 ''' ����ȊO�̏ꍇ�̓I�[�o�[���C�h�������Ĉړ��B
2474 ''' </summary>
2475 ''' <remarks>
2476 ''' Date : 2022/01/24 : M.Hayakawa
2477 ''' </remarks>
2478 Function V fnInitialZone()
2479     PC = P_Curr
2480     Ovrd 5
2481 '    ColChk Off
2482 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2483 '    Cmp Pos, &B100011
2484     If (PC.X <= PPlateLCheck.X + 1.0) And (PC.X >= PPlateLCheck.X -1.0) And (PC.Y <= PPlateLCheck.Y + 1.0) And (PC.Y >= PPlateLCheck.Y -1.0) Then
2485         If (PC.Z <= PPlateLCheck.Z + 1.0) And (PC.Z >= PPlateLCheck.Z -1.0) Then
2486             Mov PPlateLCheck_2
2487         EndIf
2488     ElseIf (PC.X <= PPlateLCheck_2.X + 1.0) And (PC.X >= PPlateLCheck_2.X -1.0) And (PC.Y <= PPlateLCheck_2.Y + 1.0) And (PC.Y >= PPlateLCheck_2.Y -1.0) Then
2489         If (PC.Z <= PPlateLCheck_2.Z + 1.0) And (PC.Z >= PPlateLCheck_2.Z -1.0) Then
2490         EndIf
2491     ElseIf (PC.X <= PPlateLGet.X + 1.0) And (PC.X >= PPlateLGet.X -1.0) And (PC.Y <= PPlateLGet.Y + 1.0) And (PC.Y >= PPlateLGet.Y -1.0) Then
2492         If (PC.Z <= PPlateLGet.Z + 1.0) And (PC.Z >= PPlateLGet.Z -1.0) Then
2493             Mov PPlateLGet_1
2494         EndIf
2495     ElseIf (PC.X <= PPlateLGet_1.X + 1.0) And (PC.X >= PPlateLGet_1.X -1.0) And (PC.Y <= PPlateLGet_1.Y + 1.0) And (PC.Y >= PPlateLGet_1.Y -1.0) Then
2496         If (PC.Z <= PPlateLGet_1.Z + 1.0) And (PC.Z >= PPlateLGet_1.Z -1.0) Then
2497         EndIf
2498     ElseIf (PC.X <= PPlateLGet_2.X + 1.0) And (PC.X >= PPlateLGet_2.X -1.0) And (PC.Y <= PPlateLGet_2.Y + 1.0) And (PC.Y >= PPlateLGet_2.Y -1.0) Then
2499         If(PC.Z <= PPlateLGet_2.Z + 1.0) And (PC.Z >= PPlateLGet_2.Z -1.0) Then
2500             Break
2501         EndIf
2502     ElseIf (PC.X <= PPlateLSet.X + 1.0) And (PC.X >= PPlateLSet.X -1.0) And (PC.Y <= PPlateLSet.Y + 1.0) And (PC.Y >= PPlateLSet.Y -1.0) Then
2503         If (PC.Z <= PPlateLSet.Z + 1.0) And (PC.Z >= PPlateLSet.Z -1.0) Then
2504             Mov PPlateLSet_1
2505             Mov PPlateLSet_2
2506         EndIf
2507     ElseIf (PC.X <= PPlateLSet_1.X + 1.0) And (PC.X >= PPlateLSet_1.X -1.0) And (PC.Y <= PPlateLSet_1.Y + 1.0) And (PC.Y >= PPlateLSet_1.Y -1.0) Then
2508       If (PC.Z <= PPlateLSet_1.Z + 1.0) And (PC.Z >= PPlateLSet_1.Z -1.0) Then
2509         Mov PPlateLSet_2
2510         EndIf
2511     ElseIf (PC.X <= PPlateLSet_2.X + 1.0) And (PC.X >= PPlateLSet_2.X -1.0) And (PC.Y <= PPlateLSet_2.Y + 1.0) And (PC.Y >= PPlateLSet_2.Y -1.0) Then
2512         If  (PC.Z <= PPlateLSet_2.Z + 1.0) And (PC.Z >= PPlateLSet_2.Z -1.0) Then
2513         EndIf
2514     ElseIf (PC.X <= PPlateRCheck.X + 1.0) And (PC.X >= PPlateRCheck.X -1.0) And (PC.Y <= PPlateRCheck.Y + 1.0) And (PC.Y >= PPlateRCheck.Y -1.0) Then
2515         If (PC.Z <= PPlateRCheck.Z + 1.0) And (PC.Z >= PPlateRCheck.Z -1.0) Then
2516         Mov PPlateLCheck_2
2517         EndIf
2518     ElseIf (PC.X <= PPlateRCheck_2.X + 1.0) And (PC.X >= PPlateRCheck_2.X -1.0) And (PC.Y <= PPlateRCheck_2.Y + 1.0) And (PC.Y >= PPlateRCheck_2.Y -1.0) Then
2519         If (PC.Z <= PPlateRCheck_2.Z + 1.0) And (PC.Z >= PPlateRCheck_2.Z -1.0) Then
2520         EndIf
2521     ElseIf (PC.X <= PPlateRGet.X + 1.0) And (PC.X >= PPlateRGet.X -1.0) And (PC.Y <= PPlateRGet.Y + 1.0) And (PC.Y >= PPlateRGet.Y -1.0) Then
2522         If (PC.Z <= PPlateRGet.Z + 1.0) And (PC.Z >= PPlateRGet.Z -1.0) Then
2523             Mov PPlateRGet_1
2524             Mov PPlateRGet_2
2525             Mov PPlateRGet_3
2526             Mov PPlateRGet_4
2527         EndIf
2528     ElseIf (PC.X <= PPlateRGet_1.X + 1.0) And (PC.X >= PPlateRGet_1.X -1.0) And (PC.Y <= PPlateRGet_1.Y + 1.0) And (PC.Y >= PPlateRGet_1.Y -1.0) Then
2529         If (PC.Z <= PPlateRGet_1.Z + 1.0) And (PC.Z >= PPlateRGet_1.Z -1.0) Then
2530             Mov PPlateRGet_2
2531             Mov PPlateRGet_3
2532             Mov PPlateRGet_4
2533         EndIf
2534     ElseIf (PC.X <= PPlateRGet_2.X + 1.0) And (PC.X >= PPlateRGet_2.X -1.0) And (PC.Y <= PPlateRGet_2.Y + 1.0) And (PC.Y >= PPlateRGet_2.Y -1.0) Then
2535         If (PC.Z <= PPlateRGet_2.Z + 1.0) And (PC.Z >= PPlateRGet_2.Z -1.0) Then
2536             Mov PPlateRGet_3
2537             Mov PPlateRGet_4
2538         EndIf
2539     ElseIf (PC.X <= PPlateRGet_3.X + 1.0) And (PC.X >= PPlateRGet_3.X -1.0) And (PC.Y <= PPlateRGet_3.Y + 1.0) And (PC.Y >= PPlateRGet_3.Y -1.0) Then
2540         If (PC.Z <= PPlateRGet_3.Z + 1.0) And (PC.Z >= PPlateRGet_3.Z -1.0) Then
2541            Mov PPlateRGet_4
2542         EndIf
2543     ElseIf (PC.X <= PPlateRGet_4.X + 1.0) And (PC.X >= PPlateRGet_4.X -1.0) And (PC.Y <= PPlateRGet_4.Y + 1.0) And (PC.Y >= PPlateRGet_4.Y -1.0) Then
2544         If (PC.Z <= PPlateRGet_4.Z + 1.0) And (PC.Z >= PPlateRGet_4.Z -1.0) Then
2545         EndIf
2546     ElseIf (PC.X <= PPlateRSet.X + 1.0) And (PC.X >= PPlateRSet.X -1.0) And (PC.Y <= PPlateRSet.Y + 1.0) And (PC.Y >= PPlateRSet.Y -1.0) Then
2547         If (PC.Z <= PPlateRSet.Z + 1.0) And (PC.Z >= PPlateRSet.Z -1.0) Then
2548             Mov PPlateRSet_1
2549             Mov PPlateRSet_2
2550         EndIf
2551     ElseIf (PC.X <= PPlateRSet_1.X + 1.0) And (PC.X >= PPlateRSet_1.X -1.0) And (PC.Y <= PPlateRSet_1.Y + 1.0) And (PC.Y >= PPlateRSet_1.Y -1.0) Then
2552         If (PC.Z <= PPlateRSet_1.Z + 1.0) And (PC.Z >= PPlateRSet_1.Z -1.0) Then
2553             Mov PPlateRSet_2
2554         EndIf
2555     ElseIf (PC.X <= PPlateRSet_2.X + 1.0) And (PC.X >= PPlateRSet_2.X -1.0) And (PC.Y <= PPlateRSet_2.Y + 1.0) And (PC.Y >= PPlateRSet_2.Y -1.0) Then
2556         If (PC.Z <= PPlateRSet_2.Z + 1.0) And (PC.Z >= PPlateRSet_2.Z -1.0) Then
2557         EndIf
2558     ElseIf (PC.X <= PPlateRSet_3.X + 1.0) And (PC.X >= PPlateRSet_3.X -1.0) And (PC.Y <= PPlateRSet_3.Y + 1.0) And (PC.Y >= PPlateRSet_3.Y -1.0) Then
2559         If (PC.Z <= PPlateRSet_3.Z + 1.0) And (PC.Z >= PPlateRSet_3.Z -1.0) Then
2560         EndIf
2561     ElseIf (PC.X <= PProductOnPltGet.X + 1.0) And (PC.X >= PProductOnPltGet.X -1.0) And (PC.Y <= PProductOnPltGet.Y + 1.0) And (PC.Y >= PProductOnPltGet.Y -1.0) Then
2562         If (PC.Z <= PProductOnPltGet.Z + 1.0) And (PC.Z >= PProductOnPltGet.Z -1.0) Then
2563             '�n���h���C�j�V�����ɖ߂�
2564             M_Out(12256) = 0    '�{�̃`���b�N��OFF
2565             M_Out(12257) = 1    '�{�̃`���b�N�JON
2566             Mov PProductOnPltGet_2
2567         EndIf
2568     ElseIf (PC.X <= PProductOnPltGet_1.X + 1.0) And (PC.X >= PProductOnPltGet_1.X -1.0) And (PC.Y <= PProductOnPltGet_1.Y + 1.0) And (PC.Y >= PProductOnPltGet_1.Y -1.0) Then
2569         If (PC.Z <= PProductOnPltGet_1.Z + 1.0) And (PC.Z >= PProductOnPltGet_1.Z -1.0) Then
2570             Mov PProductOnPltGet_2
2571         EndIf
2572     ElseIf (PC.X <= PProductOnPltGet_2.X + 1.0) And (PC.X >= PProductOnPltGet_2.X -1.0) And (PC.Y <= PProductOnPltGet_2.Y + 1.0) And (PC.Y >= PProductOnPltGet_2.Y -1.0) Then
2573         If (PC.Z <= PProductOnPltGet_2.Z + 1.0) And (PC.Z >= PProductOnPltGet_2.Z -1.0) Then
2574         EndIf
2575     ElseIf (PC.X <= PProductOnPltSet.X + 1.0) And (PC.X >= PProductOnPltSet.X -1.0) And (PC.Y <= PProductOnPltSet.Y + 1.0) And (PC.Y >= PProductOnPltSet.Y -1.0) Then
2576         If (PC.Z <= PProductOnPltSet.Z + 1.0) And (PC.Z >= PProductOnPltSet.Z -1.0) Then
2577             '�n���h���C�j�V�����ɖ߂�
2578             M_Out(12256) = 0    '�{�̃`���b�N��OFF
2579             M_Out(12257) = 1    '�{�̃`���b�N�JON
2580             Mov PProductOnPltGet_2
2581         EndIf
2582     ElseIf (PC.X <= PProductOnPltSet_1.X + 1.0) And (PC.X >= PProductOnPltSet_1.X -1.0) And (PC.Y <= PProductOnPltSet_1.Y + 1.0) And (PC.Y >= PProductOnPltSet_1.Y -1.0) Then
2583         If (PC.Z <= PProductOnPltSet_1.Z + 1.0) And (PC.Z >= PProductOnPltSet_1.Z -1.0) Then
2584             Mov PProductOnPltGet_2
2585         EndIf
2586     ElseIf (PC.X <= PProductOnPltSet_2.X + 1.0) And (PC.X >= PProductOnPltSet_2.X -1.0) And (PC.Y <= PProductOnPltSet_2.Y + 1.0) And (PC.Y >= PProductOnPltSet_2.Y -1.0) Then
2587         If (PC.Z <= PProductOnPltSet_2.Z + 1.0) And (PC.Z >= PProductOnPltSet_2.Z -1.0) Then
2588         EndIf
2589     ElseIf (PC.X <= PProductOnRoboGet.X + 1.0) And (PC.X >= PProductOnRoboGet.X -1.0) And (PC.Y <= PProductOnRoboGet.Y + 1.0) And (PC.Y >= PProductOnRoboGet.Y -1.0) Then
2590         If (PC.Z <= PProductOnRoboGet.Z + 1.0) And (PC.Z >= PProductOnRoboGet.Z -1.0) Then
2591             Mov PProductOnRoboGet_2
2592         EndIf
2593     ElseIf (PC.X <= PProductOnRoboGet_1.X + 1.0) And (PC.X >= PProductOnRoboGet_1.X -1.0) And (PC.Y <= PProductOnRoboGet_1.Y + 1.0) And (PC.Y >= PProductOnRoboGet_1.Y -1.0) Then
2594         If (PC.Z <= PProductOnRoboGet_1.Z + 1.0) And (PC.Z >= PProductOnRoboGet_1.Z -1.0) Then
2595             Mov PProductOnRoboGet_2
2596         EndIf
2597     ElseIf (PC.X <= PProductOnRoboGet_2.X + 1.0) And (PC.X >= PProductOnRoboGet_2.X -1.0) And (PC.Y <= PProductOnRoboGet_2.Y + 1.0) And (PC.Y >= PProductOnRoboGet_2.Y -1.0) Then
2598         If (PC.Z <= PProductOnRoboGet_2.Z + 1.0) And (PC.Z >= PProductOnRoboGet_2.Z -1.0) Then
2599         EndIf
2600     ElseIf (PC.X <= PProductOnRoboSet.X + 1.0) And (PC.X >= PProductOnRoboSet.X -1.0) And (PC.Y <= PProductOnRoboSet.Y + 1.0) And (PC.Y >= PProductOnRoboSet.Y -1.0) Then
2601         If (PC.Z <= PProductOnRoboSet.Z + 1.0) And (PC.Z >= PProductOnRoboSet.Z -1.0) Then
2602             Mov PProductOnRoboGet_2
2603         EndIf
2604     ElseIf (PC.X <= PProductOnRoboSet_1.X + 1.0) And (PC.X >= PProductOnRoboSet_1.X -1.0) And (PC.Y <= PProductOnRoboSet_1.Y + 1.0) And (PC.Y >= PProductOnRoboSet_1.Y -1.0) Then
2605         If (PC.Z <= PProductOnRoboSet_1.Z + 1.0) And (PC.Z >= PProductOnRoboSet_1.Z -1.0) Then
2606             Mov PProductOnRoboGet_2
2607         EndIf
2608     ElseIf (PC.X <= PProductOnRoboSet_2.X + 1.0) And (PC.X >= PProductOnRoboSet_2.X -1.0) And (PC.Y <= PProductOnRoboSet_2.Y + 1.0) And (PC.Y >= PProductOnRoboSet_2.Y -1.0) Then
2609         If (PC.Z <= PProductOnRoboSet_2.Z + 1.0) And (PC.Z >= PProductOnRoboSet_2.Z -1.0) Then
2610         EndIf
2611     Else
2612         fnMoveToEscapePosition()
2613         Break
2614     EndIf
2615     Mov PInitialPosition
2616     Cmp Off
2617     ColChk On
2618     Exit Function
2619 FEnd
2620 '
2621 '��MoveToShuntPosition
2622 ''' <summary>
2623 ''' ���݈ʒu��ǂݍ���ōŊ��̑Ҕ��ꏊ�Ɉړ�
2624 ''' </summary>
2625 ''' <remarks>
2626 ''' Date    : 2022/01/24 : M.Hayakawa
2627 ''' </remarks>
2628 Function V fnMoveToEscapePosition()
2629     PC = P_Curr
2630     Ovrd 5
2631     If Zone2(PC,PTicketRead, PTicketRead_1,10) = 1 Then
2632         Mov PTicketRead_1
2633         Break
2634     ElseIf Zone2(PC,PTicketRead_1, PProductOnPltGet_2, 20) = 1 Then
2635         Break
2636     ElseIf Zone2(PC,PProductOnPltGet_2, PInitialPosition, 20) = 1 Then
2637         Break
2638     ElseIf Zone2(PC,PProductOnPltGet_1, PProductOnPltGet, 5) = 1 Then
2639         Break
2640     ElseIf Zone2(PC,PProductOnPltGet_1, PProductOnPltGet_2, 10) = 1 Then
2641         Break
2642     ElseIf Zone2(PC,PProductOnPltGet_2, PProductOnRoboSet_2, 30) = 1 Then
2643         Break
2644     ElseIf Zone2(PC,PProductOnRoboSet_2, PProductOnRoboSet_1, 10) = 1 Then
2645         Break
2646     ElseIf Zone2(PC,PProductOnRoboSet_1, PProductOnRoboSet, 5) = 1 Then
2647         Break
2648     ElseIf Zone2(PC,PProductOnRoboSet_1, PProductOnRoboSet_2, 10) = 1 Then
2649         Break
2650     ElseIf Zone2(PC,PProductOnRoboSet_2, PPlateLGet_2, 30) = 1 Then
2651         Break
2652     ElseIf Zone2(PC,PPlateLGet_2, PInitialPosition, 10) = 1 Then
2653         Break
2654     ElseIf Zone2(PC,PPlateLGet_2, PPlateLGet_1, 10) = 1 Then
2655         Break
2656     ElseIf Zone2(PC,PPlateLGet_1, PPlateLGet, 5) = 1 Then
2657         Break
2658     ElseIf Zone2(PC,PPlateLSet_2, PPlateLSet_1, 10) = 1 Then
2659         Break
2660     ElseIf Zone2(PC,PPlateLSet_1, PPlateLSet, 5) = 1 Then
2661         Break
2662     ElseIf Zone2(PC,PPlateLSet_2, PInitialPosition, 10) = 1 Then
2663         Break
2664     ElseIf Zone2(PC,PPlateLSet_2, PPlateLCheck_2,10) = 1 Then
2665         Break
2666     ElseIf Zone2(PC,PPlateLCheck_2, PPlateLCheck, 10) = 1 Then
2667         Break
2668     ElseIf Zone2(PC,PPlateLCheck, PPlateRGet_4, 10) = 1 Then
2669         Break
2670     ElseIf Zone2(PC,PPlateRGet_4, PPlateRGet_3, 10) = 1 Then
2671         Break
2672     ElseIf Zone2(PC,PPlateRGet_3, PPlateRGet_2,30) = 1 Then
2673         Break
2674     ElseIf Zone2(PC,PPlateRGet_2, PPlateRGet_1, 10) = 1 Then
2675         Break
2676     ElseIf Zone2(PC,PPlateRGet_4, PInitialPosition, 30) = 1 Then
2677         Break
2678     ElseIf Zone2(PC,PPlateRGet_1, PPlateRGet, 5) = 1 Then
2679         Break
2680     ElseIf Zone2(PC,PPlateRGet_4, PPlateRSet_3, 10) = 1 Then
2681         Break
2682     ElseIf Zone2(PC,PPlateRSet_3, PPlateRSet_2, 10) = 1 Then
2683         Break
2684     ElseIf Zone2(PC,PPlateRSet_2, PPlateRSet_1, 10) = 1 Then
2685         Break
2686     ElseIf Zone2(PC,PPlateRSet_2, PInitialPosition, 30) = 1 Then
2687         Break
2688     ElseIf Zone2(PC,PPlateRSet_1, PPlateRSet,5) = 1 Then
2689         Break
2690     ElseIf Zone2(PC,PPlateRSet_2, PPlateRCheck_2, 10) = 1 Then
2691         Break
2692     ElseIf Zone2(PC,PPlateRCheck_2, PPlateRCheck, 5) = 1 Then
2693         Break
2694     ElseIf Zone2(PC,PPlateRCheck, PProductOnRoboGet_2, 10) = 1 Then
2695         Break
2696     ElseIf Zone2(PC,PProductOnRoboGet_2, PInitialPosition, 30) = 1 Then
2697         Break
2698     ElseIf Zone2(PC,PProductOnRoboGet_2, PProductOnRoboGet_1, 10) = 1 Then
2699         Break
2700     ElseIf Zone2(PC,PProductOnRoboGet_1, PProductOnRoboGet, 5) = 1 Then
2701         Break
2702     ElseIf Zone2(PC,PProductOnRoboGet_2, PProductOnPltSet_2, 30) = 1 Then
2703         Break
2704     ElseIf Zone2(PC,PProductOnPltSet_2, PProductOnPltSet_1, 10) = 1 Then
2705         Break
2706     ElseIf Zone2(PC,PProductOnPltSet_1, PProductOnPltSet, 5) = 1 Then
2707         Break
2708     ElseIf Zone2(PC,PProductOnPltSet_2, PTicketRead_1, 10) = 1 Then
2709         Break
2710     ElseIf Zone2(PC,PTicketRead_1, PInitialPosition, 10) = 1 Then
2711         Mov PInitialPosition
2712         Break
2713     Else
2714         fErrorProcess(11,247,281,0)
2715         Break
2716     EndIf
2717     Mov PInitialPosition
2718     Exit Function
2719 FEnd
2720 '
2721 '��fnAutoScreenComment
2722 ''' <summary>
2723 ''' ���C����ʂ̓���󋵕\��
2724 ''' �R�����gD1005�̐ݒ�
2725 ''' </summary>
2726 '''<param name="McommentD1005%">�R�����gID</param>
2727 ''' <remarks>
2728 ''' Date   : 2021/07/07 : M.Hayakawa
2729 ''' </remarks>
2730 Function fnAutoScreenComment(ByVal McommentD1005%)
2731     M_Out16(12576) = McommentD1005%
2732     Exit Function
2733 FEnd
2734 '
2735 '��InitialZoneB
2736 ''' <summary>
2737 ''' ����~��̕��A����
2738 ''' 1)���ޔ��@Z������Ɉړ�
2739 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
2740 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
2741 ''' 4)�C�j�V�����|�W�V�����ֈړ�
2742 ''' </summary>
2743 ''' <remarks>
2744 ''' Date : 2022/03/22 : N.Watanabe
2745 ''' </remarks>
2746 Function V fnInitialZoneB()
2747     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/04/27 �n��
2748 '
2749 '�p�����[�^
2750     Ovrd 5
2751 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2752 '    Cmp Pos, &B100011
2753 '
2754 '���A����J�n
2755 '
2756 '�u����Ɨ��݂͂̏ꏊ�́A�`���b�N���������    '2022/04/15 �n��
2757 *RecoveryChuckOpen
2758     PActive = P_Curr          '���݈ʒu���擾
2759     MRecoveryChuckOpen = 0    '�`���b�N����t���O ������
2760 'PProductOnRoboSet(�˂����{1�{�̒u���ʒu)�́A�`���b�N���
2761     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2762         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2763             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2764                 MRecoveryChuckOpen = 1
2765             EndIf
2766         EndIf
2767     EndIf
2768 'PProductOnRoboGet(�˂����{1�{�̎��ʒu)�́A�`���b�N���
2769     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2770         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2771             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2772                 MRecoveryChuckOpen = 1
2773             EndIf
2774         EndIf
2775     EndIf
2776 '
2777     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2778     M_Out(12256) = 0                           '�{�̃`���b�N��OFF
2779     M_Out(12257) = 1                           '�{�̃`���b�N�JON
2780 '
2781     M_20# = 0                                  'KEY���͏�����
2782     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
2783     If MRtn = 1 Then M_Out(12257) = 0          '�{�̃`���b�N�JOFF
2784     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2785 '
2786     fErrorProcess(11,244,284,0)
2787     If M_20# = MNext% Then M_20# = MClear%
2788     If M_20# = MAbout% Then GoTo *RecoveryEnd
2789     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2790     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2791 '
2792     *RecoveryChuckOpenEnd
2793 '
2794 '���ޔ�
2795     PActive = P_Curr
2796     Pmove = PActive
2797     Pmove.Z = 640           '���ޔ�����ꗥ�̍���
2798     If PActive.X > 550 Then
2799         Pmove.Z =480        '�p���b�g��ɘr��L�΂��Ă���Ƃ���640�܂ŏグ���Ȃ��ׁA��O���u
2800     EndIf
2801     If PActive.Z < Pmove.Z Then   '���݂̍�����Pmove���Ⴂ�Ƃ̂ݎ��s
2802         Mvs Pmove
2803     EndIf
2804 '
2805     Dly 1.0
2806 'J1���ȊO��ޔ��|�W�V�����ֈړ�
2807     JActive = J_Curr
2808     Jmove = JTaihi
2809     Jmove.J1 = JActive.J1        'J1���̂݌��ݒl���g�p���A���̎���JTaihi�̃|�[�Y�����
2810     Mov Jmove
2811     Dly 1.0
2812 'J1���݂̂�ޔ��|�W�V�����ֈړ�
2813     Mov JTaihi
2814     Dly 1.0
2815 '�C�j�V�����|�W�V�����ֈړ�
2816     Mov PInitialPosition
2817     Cmp Off
2818 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n
2819     If M_In(11856) = 0 Then                 ' ��~���̂�
2820         fnAutoScreenComment(501)            ' ��ԕ\��[�l�W���ߋ@�����^�]�J�n��] 2022/04/27 �n��
2821         M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
2822         MRet = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
2823         If MRet = 0 Then
2824         Else
2825             M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
2826         EndIf
2827     EndIf
2828     M_Out(12264) = 0            '�ʒu���ߏoOFF
2829     M_Out(12265) = 1            '�ʒu���ߖ�ON
2830    fErrorProcess(11,253,281,0)
2831     Exit Function
2832 *RecoveryEnd
2833 FEnd
2834 'f
2835 '
2836 '��fnRoboPosChk
2837 ''' <summary>
2838 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
2839 ''' </summary>
2840 '''<param name="MINNumber%">���͔ԍ�</param>
2841 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2842 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2843 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
2844 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
2845 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2846 ''' <remarks>
2847 ''' Date   : 2021/07/07 : M.Hayakawa
2848 ''' </remarks>
2849 Function M% fnRoboPosChk
2850     fnRoboPosChk = 0
2851     MRet = fnStepRead()
2852     '�����ʒu�łȂ��Ɣ��f�����ꍇ
2853     '�E�B���h��ʐ؊���
2854     If MRBTOpeGroupNo > 5 Then
2855         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2856         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2857         Dly 0.2
2858         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2859         Dly 1.5
2860         '
2861         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2862         '
2863         MLoopFlg% = 1
2864         While MLoopFlg% = 1
2865             '
2866             '
2867             MKeyNumber% = fnKEY_WAIT()
2868             Select MKeyNumber%
2869                 Case Is = MAbout%       '��~
2870                     M_20# = MAbout%
2871                     MLoopFlg% = -1
2872                     Break
2873                 Case Is = MNext%        '����
2874                     'MLoopFlg% = -1
2875                     Break
2876                 Case Is = MContinue%    '�p��
2877                     M_20# = MContinue%
2878                     MLoopFlg% = -1
2879                     Break
2880                 Default
2881                     Break
2882             End Select
2883         WEnd
2884     EndIf
2885     '
2886     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
2887         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2888         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
2889         Select MRBTOpeGroupNo
2890             Case Is = 5                          '�������Ȃ�
2891                 Break
2892             Case Is = 10                         '�����ʒu�֖߂�
2893                 'Mov PTEST001
2894                 Break
2895             Case Is = 15                         '�����ʒu�֖߂�
2896                 'Mov PTEST002
2897                 Dly 0.5
2898                 'Mov PTEST001
2899                 Dly 0.5
2900                 Break
2901             Default
2902                 Break
2903         End Select
2904         '
2905         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
2906         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
2907         MRBTOpeGroupNo = 5
2908         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
2909         Dly 1.0
2910         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
2911         fnRoboPosChk = 1                        '�����ʒu������s
2912         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2913     EndIf
2914     Exit Function
2915 FEnd
2916 '
2917 '��frInCheck
2918 ''' <summary>
2919 ''' �Z���T�[IN�`�F�b�N
2920 ''' </summary>
2921 '''<param name="MINNumber%">���͔ԍ�</param>
2922 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2923 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2924 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2925 ''' <remarks>
2926 ''' Date   : 2021/07/07 : M.Hayakawa
2927 ''' </remarks>
2928 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2929     M_Timer(4) = 0
2930     MloopFlg = 0
2931     While MloopFlg = 0
2932         MCrtTime& = M_Timer(4)
2933         If M_In(MINNumber%) = MCMPFLG% Then
2934             MloopFlg = 1
2935             frInCheck = 1
2936         ElseIf MCrtTime& > MTimeCnt& Then
2937             MloopFlg = 1
2938             frInCheck = 0
2939         EndIf
2940     WEnd
2941     Exit Function
2942 FEnd
2943 '-----------------------------------------------
2944 '
2945 '�˂����ߋ@�ʐM�m�F
2946 '
2947 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2948 'fScewTcomChk = 0�@�F����I��
2949 '          �@�@ -1 �F�ُ�I��
2950 '-----------------------------------------------
2951 Function M% fScewTcomChk
2952 *ReCheckScewTcomChk
2953     fScewTcomChk = 0
2954     '�ʐM�m�F���M
2955     M_Out(MOUT_ScwT_ComChk%) = MOn%
2956     '�ʐM�m�F��M�ҋ@
2957 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
2958     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
2959     '�ʐM�m�F���M�I��
2960     M_Out(MOUT_ScwT_ComChk%) = MOff%
2961     If MRtn = 0 Then
2962         fScewTcomChk = -1
2963     EndIf
2964     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
2965  '
2966 FEnd
2967 '
2968 '
2969 '-----------------------------------------------
2970 '
2971 '�˂����ߊJ�n���M
2972 '
2973 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2974 'fScewTStart = 0�@�F����I��
2975 '          �@�@-1 �F�ُ�I��
2976 '-----------------------------------------------
2977 Function M% fScewTStart
2978     fScewTStart = 0
2979     nRet% = 0
2980     '�˂����ߊJ�n�ҋ@����M
2981 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
2982     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
2983     If MRtn = 0 Then nRet% = -1
2984     If MRtn = 0 Then GoTo *ScrewStartERROR      '�J�n�ł��Ȃ������ꍇ�W�����v
2985     Dly 0.1
2986     '�˂����ߊJ�n��M�𑗐M
2987     M_Out(MOUT_ScwT_ST%) = MOn%
2988     Dly 0.5
2989     'Wait M_In(MTEST_KEY%) = MOn%
2990     '�˂����ߊJ�n���M�I��
2991     M_Out(MOUT_ScwT_ST%) = MOff%
2992     '
2993 *ScrewStartERROR
2994     fScewTStart = nRet%
2995 FEnd
2996 '
2997 '
2998 '
2999 '-----------------------------------------------
3000 '
3001 '�˂����ߊ�����M
3002 '
3003 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3004 'fScewTcomChk = 0�@�F����I��
3005 '          �@ �@-1 �F�ُ�I��
3006 '-----------------------------------------------
3007 Function M% fScewTFinish
3008 *ReCheckScewTFinish
3009     fScewTFinish = 0
3010     '�˂����ߊ����ҋ@����M
3011 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3012     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3013     If MRtn = 0 Then
3014         fScewTFinish = -1
3015     EndIf
3016     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3017     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3018     Dly 0.1
3019     '�˂����ߊ�����M�𑗐M
3020     M_Out(MOUT_ScwT_FinOK%) = MOn%
3021     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3022     '�˂����ߊJ�n���M�I��
3023     M_Out(MOUT_ScwT_FinOK%) = MOff%
3024     'Wait M_In(MTEST_KEY%) = MOn%
3025     '
3026 *ScewTFinish_ErrEnd
3027 FEnd
3028 '
3029 '
3030 '-----------------------------------------------
3031 '
3032 '����xx��~��M
3033 '
3034 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3035 'fScewTCaseStop = 0�@�F����I��
3036 '          �@   �@-1 �F�ُ�I��
3037 '-----------------------------------------------
3038 Function M% fScewTCaseStop(ByVal MCase%())
3039 *ReCheckScewTCaseStop
3040     fScewTCaseStop = 0
3041     '����xx��~����M
3042     Wait M_In(MCase%(1)) = MOn%
3043     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3044     If MRtn = 0 Then
3045         fScewTCaseStop = -1
3046     EndIf
3047     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3048     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3049     Dly 0.1
3050     '����xx��~��M�𑗐M
3051     M_Out(MCase%(2)) = MOn%
3052     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3053     '�˂����ߊJ�n���M�I��
3054     M_Out(MCase%(2)) = MOff%
3055 *ScewTCaseStop_ErrEnd
3056     '
3057 FEnd
3058 '
3059 '-----------------------------------------------
3060 '
3061 '�ĊJ�n��M
3062 '
3063 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3064 'fScewTReStart = 0�@�F����I��
3065 '              �@-1 �F�ُ�I��
3066 '-----------------------------------------------
3067 Function M% fScewTReStart()
3068 *ReCheckScewTReStart
3069     fScewTReStart = 0
3070     '�ĊJ�n����M
3071     Wait M_In(MIN_ScwT_ReST%) = MOn%
3072     MRtn = fTimeOutJudge(MIN_ScwT_ReST%,MOn%)
3073     If MRtn = 2 Then GoTo *ReCheckScewTReStart
3074     If MRtn = 0 Then GoTo *ScewTReStart_ErrEnd
3075     Dly 0.1
3076     '�ĊJ�n��M�𑗐M
3077     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msec�p���X
3078 *ScewTReStart_ErrEnd
3079     Exit Function
3080 FEnd
3081 '
3082 '��fScrewTighenRoboCheck
3083 '<summary>
3084 '�˂����{�Ď�
3085 '</summary>
3086 '<param name = "MStopNum%"> ��~�ԍ�</param>
3087 '<returns>���� 0:�˂����{�ُ�I�� 1:OK </returns>
3088 '<make>
3089 '2021/12/2 �����V��
3090 '</make>
3091 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3092     fnAutoScreenComment(503)    '��ԕ\��[�˂����{����I���҂�] 2022/04/27 �n��
3093     fScrewTighenRoboCheck = 1
3094     MScrewTighenRoboFlg% = 1    '�t���O�̏�����
3095     MCheck% = 0
3096     While MScrewTighenRoboFlg% = 1
3097         MCheck% = M_In16(11904)
3098         If M_In(MStopNum%) = 1 Then '��~�ʒu�܂ŗ�����
3099             MScrewTighenRoboFlg% = 0 '�֐��𔲂���
3100             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/27 �n��
3101         EndIf
3102         If MCheck% <> 0 Then
3103             fScrewTighenRoboError(MCheck%)
3104             Select M_20#
3105                 Case MAbout%            '��~�������ꂽ�ꍇ
3106                     M_Out(12869) = 1 Dly 1.0
3107                     MScrewTighenRoboFlg% = 0
3108                     fScrewTighenRoboCheck = 0   '�ُ�I��
3109                     Break
3110                 Case MNgProcess%        'NG�������ꂽ�ꍇ
3111                     M_Out(12873) = 1 Dly 1.0
3112                     MScrewTighenRoboFlg% = 0
3113                     fScrewTighenRoboCheck = 0   '�ُ�I��
3114                     Break
3115                 Case MContinue%             '���g���C�������ꂽ�ꍇ
3116                     M_20# = MClear%         'M_20#������
3117                     M_Out(12871) = 1 Dly 1.0
3118                     Break
3119                 Case MNext%                 '���ւ������ꂽ�ꍇ
3120                     M_20# = MClear%         'M_20#������
3121                     M_Out(12874) = 1 Dly 1.0
3122                     Break
3123             End Select
3124             Dly 0.5
3125         EndIf
3126     WEnd
3127     Exit Function
3128 FEnd
3129 '��fScrewTighenRoboError
3130 '<summary>
3131 '�˂����{�G���[����
3132 '</summary>
3133 '<param name = "ErrorCode%"> �G���[�ԍ�</param>
3134 '<make>
3135 '2021/12/2 �����V��
3136 '</make>
3137 Function fScrewTighenRoboError(ErrorCode%)
3138     MCommentD1001 = ErrorCode% + 300
3139     fErrorProcess(11,MCommentD1001,0,0)
3140     Exit Function
3141 FEnd
3142 '��fErrorProcess
3143 '<summary>
3144 '�G���[����
3145 '</summary>
3146 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
3147 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
3148 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
3149 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
3150 '<make>
3151 '2021/11/5 �����V��
3152 '</make>
3153 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3154     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
3155     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
3156     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
3157     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
3158 *RETRY_ERR_PROCESS
3159      M_20# = MClear%     '������
3160 '        '�G���[�����L�q
3161         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3162 '        'GOT KEY���͑҂�
3163         MKeyNumber = fnKEY_WAIT()
3164 '        '
3165         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
3166             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3167  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3168             Break
3169          '
3170         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
3171             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3172  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3173             Break
3174         '
3175         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
3176             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3177  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3178             Break
3179          '
3180         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
3181             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3182  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3183             Break
3184         '
3185         EndIf
3186         '
3187         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3188         fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3189     Exit Function
3190 FEnd
3191 '
3192 '��fnTorqueCheck
3193 ''' <summary>
3194 ''' �g���N�`�F�b�N����p�̃��C��
3195 ''' </summary>
3196 ''' <remarks>
3197 ''' Date   : 2021/12/21 : H.AJI
3198 ''' </remarks>'
3199 Function M% fnTorqueCheck
3200     '�g���N�`�F�b�N�����M  �����n��~
3201     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3202     '
3203     fnTorqueCheck = 0
3204     Ovrd 20
3205     'Mov PInitialPosition              '�����ʒu�ړ�
3206     Ovrd 100
3207     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3208     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3209     Dly 0.2
3210     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3211     '
3212     'M6340  �g���N�`�F�b�N��M
3213     'Dly 5.0
3214     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
3215     Dly 1.0
3216     M_Out(12340) = 0
3217     '
3218     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
3219     M_Out(12835) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3220    Wait M_In(11843) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3221     M_Out(12835) = 0                         '�˂����{�g���N�`�F�b�N��ʐؑ֊���
3222     '
3223     '
3224     MLoopFlg = 1
3225     While MLoopFlg = 1
3226         '
3227         'Mov PInitialPosition              '�����ʒu�ړ�
3228         '
3229         MKeyNumber = fnKEY_WAIT()
3230         Select MKeyNumber
3231             Case Is = 1           '��~
3232                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
3233                 Dly 1.0
3234                 M_Out(12343) = 0
3235                 Ovrd 20
3236                 'Mov PTicketRead_1
3237                 M_Out(12840) = 1          '�g���N�`�F�b�N�I��
3238                 Wait M_In(11859) = 1      '�˂����{����̏I��
3239                 M_Out(12840) = 0          '�g���N�`�F�b�N�I��
3240                 Ovrd 100
3241                 M_20# = 1
3242                 MLoopFlg = -1
3243                 Break
3244             Case Is = 2           '����
3245                 Break
3246             Case Is = 3           '�p��
3247                 Break
3248             Case Is = 4           '�g���N�`�F�b�N�J�n
3249                 M_Out(12342) = 1          '�g���N�`�F�b�N�J�n�v����M M6342
3250                 Dly 1.0
3251                 M_Out(12342) = 0
3252                 If M_In(11862) = 1 Then             '�g���N�`�F�b�J�[�m�F
3253                     fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3254                     MRet = fnScrewMTorque()           '�˂����{�p�g���N�`�F�b�N
3255                 EndIf
3256                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3257                 'MRet = fnMoveTorquePosi()
3258                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
3259                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3260                 Break
3261             Default
3262                 Break
3263         End Select
3264     WEnd
3265     '
3266     '�g���N�`�F�b�N����~���M
3267     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3268     '
3269     '���{�b�g�̈ʒu�����ɖ߂�
3270     '
3271     Exit Function
3272  FEnd
3273  '
3274 '
3275 '
3276 '---------------------------
3277 '
3278 '    ���C����ʂ̕\���A��\���ݒ�
3279 '         �R�����gD1001, D1002, D1003�̐ݒ�
3280 '           MWindReSet = 0     ��ʔ�\��
3281 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
3282 '           MWindErrScr = 10    �G���[��� D1001, D1002
3283 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
3284 '
3285 '---------------------------
3286 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3287     fnMainScreenOpen = 0
3288     '
3289    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3290         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
3291     EndIf
3292     '
3293     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3294         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
3295     EndIf
3296     '
3297     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3298         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
3299     EndIf
3300     '
3301     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
3302     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
3303     Dly 0.5
3304     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
3305     Exit Function
3306 FEnd
3307 '
3308 '��Main
3309 ''' <summary>
3310 ''' �g���N�`�F�b�N������
3311 ''' </summary>
3312 ''' <remarks>
3313 ''' Date   : 2021/12/21 : H.AJI
3314 ''' </remarks>'
3315 Function M% fnScrewMTorque
3316     fnScrewMTorque = 0
3317     M_Out(12838) = 1                         '�g���N�`�F�b�N�J�n1
3318     Wait M_In(11857) = 1                     '��M����
3319     M_Out(12838) = 0                         '�g���N�`�F�b�N�J�n1
3320     Dly 2.0
3321     Exit Function
3322 FEnd
3323 '
3324 '
3325 '----------------------------------------------------------------
3326 'fTimeOutJudge
3327 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3328 '����
3329 'Address% = �Ď��A�h���X�ԍ�
3330 'JudgeFlg% = �ΏۃA�h���X�̐���I�����̒l
3331 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3332 '�߂�l = 0 �G���[
3333 '         1 ����I��
3334 '         2 ���g���C
3335 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3336 '�쐬��
3337 '2022/9/20 ����
3338 '----------------------------------------------------------------
3339 '
3340 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3341     fTimeOutJudge = 0
3342     MJudge% = 1
3343     MRtn = 0
3344     M_20# = MClear%
3345     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3346 *TimeOutLoop
3347     If MRtn = 1 Then GoTo *TimeOut
3348         fErrorProcess(11,202,203,0)
3349         If M_20# = MNext% Then GoTo *TimeOutLoop
3350         If M_20# = MContinue% Then MJudge% = 2
3351         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3352 *TimeOut
3353     fTimeOutJudge = MJudge%
3354 '
3355 *JUDGE_ERROR_END
3356 FEnd
3357 '
3358 '��Main
3359 ''' <summary>
3360 ''' �g������p�̃��C��
3361 ''' </summary>
3362 ''' <remarks>
3363 ''' Date   : 2021/07/07 : M.Hayakawa
3364 ''' </remarks>'
3365 Function Main
3366     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
3367     '
3368     If M_Svo=0 Then
3369         Servo On
3370     EndIf
3371     Wait M_Svo=1
3372 '�g���X�^�[�g���t�����v���p���XON
3373     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3374 '�p�g���C�g����
3375     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
3376     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
3377     '
3378     M_20# = 0                                   'KEY���͏�����
3379     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
3380     MRet% = 0
3381 '�����ʒu�̊m�F�ƈړ�
3382 '
3383 '���A����@���s�E�����s����      2022/03/22 �n�� �쐬
3384     PActive = P_Curr                    '���݈ʒu���擾
3385     MRecoveryPass% = 0
3386     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3387         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3388             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3389                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
3390             EndIf
3391         EndIf
3392     EndIf
3393     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3394         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3395             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3396                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
3397             EndIf
3398         EndIf
3399     EndIf
3400     If MRecoveryPass% = 0 Then
3401        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
3402     EndIf
3403 '
3404 '
3405 '
3406     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
3407         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
3408 '�g���N�`�F�b�N
3409         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3410             MRet% = fnTorqueCheck()
3411             Break
3412         Else
3413 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
3414 '                MRtn = InspInit()               '�摜��������������
3415 '            EndIf
3416             '
3417            M_20# = MClear%                    '������
3418 '�g���J�n
3419             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3420                 MRet% = fnAssyStart()
3421             Else
3422                 M_20# = MPass%
3423             EndIf
3424 '�g���I�����t����
3425             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3426             Wait M_In(11572) = 1            '���t�擾����
3427             Dly 0.1
3428             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3429 '���t�^�[���j�b�g�ւ�OUT
3430             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3431             fnAutoScreenComment(89)         'AUTO��� �g����������
3432             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3433 'OK/NG�t���O�o��
3434             If M_20# <= 0 Then
3435                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3436             ElseIf M_20# = MPass% Then
3437                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3438             EndIf
3439 'PIAS�ɑg������������
3440             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3441                 If M_20# = MPass% Then
3442                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3443                 Else
3444                     'KEY���͂�NG�̏ꍇ
3445                     If M_20# = MNgProcess% Then
3446                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3447                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3448                         MRet% = fnPiasWrite(MNG%)
3449                        nAssyNgQty = nAssyNgQty + 1
3450                     EndIf
3451                     '
3452                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3453                     If M_20# = MAssyOK% Then
3454                             '-----------------------
3455                             'D732 -> D2600 �R�s�[�v��
3456                             M_Out(12566) = 1
3457 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3458                             M_Out(12566) = 0
3459                             '
3460                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3461                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3462                             '��ԍ��ƍ�(PP�͖��g�p�j
3463 '                            MRet% = fnPCBNumberCheck()
3464                         Else
3465                             MRet% = 1
3466                         EndIf
3467                         '
3468                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3469                             If M_20# <> MAbout% Then
3470                                 '�H������OK��������
3471                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3472                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3473                                 MRet% = fnPiasWrite(MOK%)
3474                                 nAssyOkQty = 0
3475                                 nAssyOkQty = nAssyOkQty + 1
3476                             Else
3477                                 nAssyOkQty = nAssyOkQty + 1
3478                             EndIf
3479                         EndIf
3480                     EndIf
3481 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3482 '                    MRet% = fnPiasWrite(MOK%)
3483                 EndIf
3484             Else
3485                 nAssyOkQty = nAssyOkQty + 1
3486             EndIf
3487             '
3488             '�g���I�����t��������
3489             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3490             '�������A�g��OK���A�g��NG��������
3491 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3492             '
3493 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3494 '                '�摜�����I������
3495 '                MRtn = InspQuit()
3496 '            EndIf
3497         EndIf
3498         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3499     EndIf
3500 '�p�g���C�g����
3501     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3502     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3503 'GOT�\��
3504     fnAutoScreenComment(93)  'AUTO��� �H������
3505 FEnd
3506 End
3507 '
3508 '���܂��Ȃ��R�����g
3509 '��΍폜�����
3510 '
3511 ''
3512 '
3513 '
3514 '
PInspPosition(1)=(-55.67,-374.21,+601.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(11)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(12)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(13)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(14)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(15)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(16)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(17)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(18)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(19)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(20)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(21)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(22)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(23)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(24)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(25)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(26)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(27)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(28)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(29)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(30)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PTemp=(+599.21,-290.97,+540.00,+180.00,+0.00,-90.00,+0.00,+0.00)(7,0)
PScrewPos(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PActive=(+599.21,-290.97,+540.00,+180.00,+0.00,-90.00)(7,0)
Pmove=(+45.33,-320.00,+640.00,-180.00,+0.00,-178.68)(7,0)
PInitialPosition=(+350.00,+0.00,+540.00,+180.00,+0.00,+180.00)(7,0)
PPlateLCheck=(-53.94,-594.69,+601.00,-180.00,+0.00,+90.00)(7,0)
PPlateLCheck_2=(-53.94,-594.69,+631.00,+180.00,+0.00,+90.00)(7,0)
PPlateLGet=(+638.90,-161.08,+442.45,+179.20,+0.10,+90.43)(7,0)
PPlateLGet_1=(+638.90,-161.08,+460.00,+179.20,+0.10,+90.43)(7,0)
PPlateLGet_2=(+396.00,-160.55,+600.00,+179.20,+0.10,+90.43)(7,0)
PPlateLSet=(+49.40,-583.75,+544.65,+179.96,-0.32,-179.72)(7,0)
PPlateLSet_1=(+49.40,-583.75,+590.00,+179.96,-0.32,-179.72)(7,0)
PPlateLSet_2=(+44.56,-280.00,+640.60,+179.98,-0.12,-179.36)(7,0)
PPlateRCheck=(-55.67,-374.21,+601.00,-180.00,+0.00,+90.00)(7,0)
PPlateRCheck_2=(-55.67,-374.21,+631.00,-180.00,+0.00,+90.00)(7,0)
PPlateRGet=(-516.84,+69.66,+362.89,-179.65,-0.45,+0.40)(7,15)
PPlateRGet_1=(-517.83,+70.10,+400.00,-179.65,-0.45,+1.26)(7,15)
PPlateRGet_2=(-323.09,-0.06,+640.58,+179.98,-0.13,+1.25)(7,0)
PPlateRGet_3=(-271.20,+14.83,+640.56,+180.00,+0.00,-3.13)(7,15)
PPlateRGet_4=(+0.00,-271.61,+640.53,+180.00,+0.00,+90.00)(7,0)
PPlateRSet=(+51.00,-523.50,+543.12,-179.63,-0.70,-179.80)(7,0)
PPlateRSet_1=(+51.00,-523.50,+570.00,-179.63,-0.70,-179.80)(7,0)
PPlateRSet_2=(+45.33,-320.00,+640.00,+180.00,+0.00,-178.68)(7,0)
PPlateRSet_3=(+0.01,-336.64,+640.48,+180.00,-0.01,+90.00)(7,0)
PProductOnPltGet=(+547.57,-99.50,+414.27,-180.00,+0.00,-179.80)(7,0)
PProductOnPltGet_1=(+547.57,-99.50,+460.00,-180.00,+0.00,-179.80)(7,0)
PProductOnPltGet_2=(+547.57,-99.50,+530.00,-180.00,+0.00,-179.80)(7,0)
PProductOnPltSet=(+547.55,-98.19,+413.10,+179.93,+0.00,-179.80)(7,0)
PProductOnPltSet_1=(+547.55,-98.19,+460.00,-179.93,+0.00,-179.80)(7,0)
PProductOnPltSet_2=(+547.57,-98.90,+530.00,-180.00,+0.00,-179.80)(7,0)
PProductOnRoboGet=(+103.80,-555.30,+466.95,+179.84,-0.08,-179.68)(7,0)
PProductOnRoboGet_1=(+103.80,-555.30,+500.00,+179.84,-0.08,-179.68)(7,0)
PProductOnRoboGet_2=(+103.80,-555.30,+640.00,+179.84,-0.08,-179.68)(7,0)
PProductOnRoboSet=(+103.80,-555.30,+466.95,+179.84,-0.08,-179.68)(7,0)
PProductOnRoboSet_1=(+103.80,-555.30,+510.00,+179.84,-0.08,-179.68)(7,0)
PProductOnRoboSet_2=(+103.80,-555.30,+640.00,+179.84,-0.08,-179.68)(7,0)
PTicketRead=(+599.21,-290.97,+473.00,-180.00,+0.00,-90.00)(7,0)
PTicketRead_1=(+599.21,-290.97,+540.00,-180.00,+0.00,-90.00)(7,0)
JActive=(-81.94,-8.46,+107.89,+0.00,+80.57,-83.26)
Jmove=(-81.94,-9.85,+108.99,+0.00,+80.50,+0.00)
JTaihi=(+0.00,-9.85,+108.99,+0.00,+80.50,+0.00)
