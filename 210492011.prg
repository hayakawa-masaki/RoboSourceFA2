1 ' ===================================
2 '
3 '  21049001 STEP5 Assy1プログラム
4 '
5 ' 作成者：自動化T
6 ' 作成日：2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1から流用
8 ' Ver 0.3 2021.12.22 画像検査関数ISInspection→ISInspectionSingle、画像検査追加 file:210542003
9 ' ===================================
10 '===== <Insight定数> =====
11 '===== <Insight変数定義> =====
12 Dim PInspPosition(30)               '画像処理Function引渡し用位置変数
13 Dim MInspGroup%(30)                 '画像処理Function引渡し用変数
14 Def Inte MIN_IS_Ready               '【入力IO】Insight準備OK
15 Def Inte MIN_IS_JobLoadOK           '【入力IO】Insightジョブロード正常終了
16 Def Inte MIN_IS_JobLoadNG           '【入力IO】Insightジョブロード異常終了
17 Def Inte MIN_IS_InspGSetOK          '【入力IO】Insight検査グループ番号設定正常終了
18 Def Inte MIN_IS_InspGSetNG          '【入力IO】Insight検査グループ番号設定異常終了
19 Def Inte MIN_IS_InspOK              '【入力IO】Insight検査OK
20 Def Inte MIN_IS_InspNG              '【入力IO】Insight検査NG
21 Def Inte MIN_IS_InspErr             '【入力IO】Insight検査異常終了
22 Def Inte MIN_IS_InspCapDone         '【入力IO】Insight検査画像取込完了
23 '
24 Def Inte MIN_IS_ErrNum              '【入力IO】Insight処理エラー番号取得開始アドレス(16bit)
25 'Output Signal
26 Def Inte MOUT_IS_JobLoadReq         '【出力IO】Insight JOBロード要求
27 Def Inte MOUT_IS_InspGSetReq        '【出力IO】Insight 検査グループ番号設定要求
28 Def Inte MOUT_IS_Insp               '【出力IO】Insight 検査実行要求
29 '
30 Def Inte MOUT_IS_JobNum             '【出力IO】Insight JOB番号設定開始アドレス(16bit)
31 Def Inte MOUT_IS_InspGNum           '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
32 '
33 Def Inte MOUT_InspErrNum            '【出力IO】検査実行エラー番号開始アドレス(16bit)
34 Def Inte MOUT_InspNGStepNum         '【出力IO】検査実行NGStep番号開始アドレス(16bit)
35 '作業用変数
36 Def Inte MInspErrNum                '検査実行エラー番号
37 Def Inte MInspNGStepNum             '検査実行NGStep番号
38 Def Inte MRtn                       'Function戻り値取得用
39 Def Inte MRtn2                      'Function戻り値取得用
40 Def Inte MRet3                      'Function戻り値取得用
41 Def Inte MGRtn                      'Function戻り値取得用 ネジ供給機
42 Def Inte MInspErrNumSub             '検査実行エラー番号sub　20190820追加
43 Def Inte MovrdA                     'ネジ締めOvrd 可変用   20191127追加
44 Def Float MSpdA                     'ネジ締めSpd　可変用   20191127追加
45 Def Pos PTemp                       'ネジ締め上空位置計算用    20200312追加'
46 MovrdA% = 20                        'ネジ締めOvrd 可変用   20191127追加
47 MSpdA = 800                        'ネジ締めSpd　可変用   20191127追加
48 '===== <Insight変数設定> =====
49 MIN_IS_Ready%        =   11380      '【入力IO】Insight準備OK
50 MIN_IS_JobLoadOK%    =   11381      '【入力IO】Insightジョブロード正常終了
51 MIN_IS_JobLoadNG%    =   11382      '【入力IO】Insightジョブロード異常終了
52 MIN_IS_InspGSetOK%   =   11383      '【入力IO】Insight検査グループ番号設定正常終了
53 MIN_IS_InspGSetNG%   =   11384      '【入力IO】Insight検査グループ番号設定異常終了
54 MIN_IS_InspOK%       =   11385      '【入力IO】Insight検査OK
55 MIN_IS_InspNG%       =   11386      '【入力IO】Insight検査NG
56 MIN_IS_InspErr%      =   11387      '【入力IO】Insight検査異常終了
57 MIN_IS_InspCapDone%  =   11388      '【入力IO】Insight検査画像取込完了
58 MIN_IS_ErrNum%       =   11408      '【入力IO】Insight処理エラー番号開始アドレス(16bit)
59 'Output Signal
60 MOUT_IS_JobLoadReq%  =   12370      '【出力IO】Insight JOBロード要求
61 MOUT_IS_InspGSetReq% =   12371      '【出力IO】Insight 検査グループ番号設定要求
62 MOUT_IS_Insp%        =   12372      '【出力IO】Insight 検査実行要求
63 MOUT_IS_JobNum%      =   12384      '【出力IO】Insight JOB番号設定開始アドレス(16bit)
64 MOUT_IS_InspGNum%    =   12400      '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
65 MOUT_InspErrNum%     =   12416      '【出力IO】検査実行エラー番号開始アドレス(16bit)
66 MOUT_InspNGStepNum%  =   12432      '【出力IO】検査実行NGStep番号開始アドレス(16bit)
67 '===== <電ドラ定数> =====
68 '===== <電ドラ変数定義> =====
69 X20_Driver=11248                    '電ドラステイタス1　Driver Status 1
70 X21_Driver=11249 '電ドラステイタス2  Driver Status 2
71 X22_Driver=11250 '電ドラステイタス3  Driver Status 3
72 X23_Driver=11251 '電ドラステイタス4  Driver Status 4
73 X24_Driver=11252 '電ドラエラーメッセージ1 Driver Error E1
74 X25_Driver=11253 '電ドラエラーメッセージ2 Driver Error E2
75 X26_Driver=11254 '電ドラエラーメッセージ3 Driver Error E3
76 X27_Driver=11255 '電ドラエラーメッセージ4 Driver Error E4
77 X28_Driver=11256 '電ドラトータルエラーシグナル Total Error
78 X29_Driver=11257 '電ドラ終了シグナル Comlete signal
79 X2A_Driver=11258 '電ドラエラーメッセージ5 Driver Error E5
80 '11584   'toRBトルクドライバ-COMP_ERR送信
81 Y60_Driver=12240 '電ドラ半時計回り CCW
82 Y61_Driver=12241 '電ドラ時計回り CW
83 Y62_Driver=12242 'バンクセッティング BANK C1
84 Y63_Driver=12243 'バンクセッティング BANK C2
85 Y64_Driver=12244 'バンクセッティング BANK C3
86 Y65_Driver=12245 'プログラムセッティング PRG SET F1
87 Y66_Driver=12246 'プログラムセッティング PRG SET F2
88 Y67_Driver=12247 'プログラムセッティング PRG SET F3
89 '組立2
90 X34_NG1=11268 'ねじっこ1　Read
91 X35_NG2=11269 'ねじっこ2　Read
92 '組立3
93 X3F_NG1=11279 'ねじっこ1　Read
94 '
95 Dim PScrewPosTemp(10)                                               'ネジ締め用Function引数変数
96 Dim PGetScrewPosTemp(10)                                            'ねじ供給機からねじを得るFunction引数変数
97 Dim PEscapePosi(10)
98 MLoopCnt% = 0'
99 '===== <ロボット定数> =====
100 '===== <ロボット変数定義> =====
101 MRBTOpeGroupNo = 0                    'ロボット動作番号初期化
102 MCommentD1001 = 0
103 MCommentD1002 = 0
104 MCommentD1003 = 0
105 MScreenNo = 0
106 '
107 MCommentTSU = 0
108 MCommentTSD = 0
109 'ウィンド画面番号設定
110 MWindReSet = 0
111 MWindInfoScr = 5
112 MWindErrScr = 10
113 MWindErrScr2 = 11
114 MWindErrScr3 = 13
115 MWindErrScr17 = 17
116 MWindErrScr18 = 18
117 MWindCmmnScr = 20
118 MWindJigRelase19049 = 60
119 MWindJigRelase19050 = 61
120 MWindJigRelase19051 = 62
121 '
122 MClear% = 0        'KEY_のクリア
123 MAbout% = 1        'KEY_停止
124 MNext% = 2         'KEY_次のステップへ移行
125 MContinue% = 3     'KEY_継続 再度同じ動作を行う
126 '
127 Def Inte MNgProcess
128 MNgProcess% = 5      'KEY_NG
129 '
130 MAssyOK% = 6       '組立完了
131 MPass% = 7         '工程パス
132 MPiasNG% = 8       'Pias確認時履歴NG
133 '
134 '初期化用KEY番号   '
135 MRobotInit1% = 11  '初期位置用
136 MRobotInit2% = 12  '初期位置用
137 MRobotInit3% = 13  '初期位置用
138 MRobotInit4% = 14  '初期位置用
139 '
140 MIN_INIT1REQUEST% = 11568 'toRBT_ロボット初期位置1要求
141 MIN_INIT2REQUEST% = 11569 'toRBT_ロボット初期位置2要求
142 MIN_INIT3REQUEST% = 11570 'toRBT_ロボット初期位置3要求
143 MIN_INIT4REQUEST% = 11571 'toRBT_ロボット初期位置4要求
144 '
145 MOUT_INIT1RECIVE% = 12560 'toPLC_ロボット初期位置1受信
146 MOUT_INIT2RECIVE% = 12561 'toPLC_ロボット初期位置2受信
147 MOUT_INIT3RECIVE% = 12562 'toPLC_ロボット初期位置3受信
148 MOUT_INIT4RECIVE% = 12563 'toPLC_ロボット初期位置4受信
149 '
150 MopeNo = 0
151 '
152 MOK% = 1               '各判定用
153 MNG% = 0               '各判定用
154 MTIMEOUT% = -1         '各判定用
155 MJudge% = 0            '判定情報格納用
156 '
157 '
158 MRECIVETIME& = 0
159 MSETTIMEOUT10& = 10000&                '10秒設定
160 MSETTIMEOUT03& = 3000&                 '3秒設定
161 MSETTIMEOUT01& = 1000&                 '1秒設定
162 MSETTIMEOUT05& = 5000&                 '5秒設定
163 MSETTIMEOUT009& = 900&                 '0.9秒設定
164 MSETTIMEOUT008& = 800&                 '0.8秒設定
165 MSETTIMEOUT007& = 700&                 '0.7秒設定
166 MSETTIMEOUT006& = 600&                 '0.6秒設定
167 MSETTIMEOUT005& = 500&                 '0.5秒設定
168 MSETTIMEOUT004& = 400&                 '0.4秒設定
169 MSETTIMEOUT003& = 300&                 '0.3秒設定
170 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
171 MIN_PIAS_ComOK% = 11552                'PC通信OK
172 MIN_PIAS_ComTimeOut% = 11576           'PC通信確認タイムアウト
173 MIN_PIAS_ComNG% = 11553                'PC通信NG
174 MOUT_PIAS_ComCheck% = 12544            'PC通信確認要求
175 MOUT_PIAS_Missing_Process% = 12546     '工程抜け確認要求
176 MIN_PIAS_ModelTypeNG% = 11554          'モデル仕向NG
177 MIN_PIAS_ProcessHistryNG% = 11555      '前工程履歴NG
178 MIN_PIAS_ProcessHistryOK% = 11556      '前工程履歴OK
179 MIN_PIAS_ProcessHistryErr% = 11557     '工程履歴処理エラー
180 MIN_PIAS_MyProcessComp% = 11573        '自工程履歴あり
181 MIN_PIAS_ProcessHistryTimeOut% = 11578 '工程履歴タイムアウト
182 MOUT_OKNG% = 12549                     'PLC OUT でOK=1, NG=0 出力
183 '
184 MOUT_PiasPCBNumberCheck = 12557        '基板番号照合
185 MIN_PiasPCBNumberOK% = 11566          '基板番号OK
186 MIN_PiasPCBNumberNG% = 11565          '基板番号NG
187 MIN_PiasPCBNumberErr% = 11567         '基板番号処理エラー
188 '
189 MOUT_PiasAssyResultOK% = 12549    '組立OK
190 MOUT_PiasAssyResultNG% = 12550    '組立NG
191 MOUT_PiasAssyResultWr% = 12548    '工程履歴書き込み
192 '
193 MIN_PiasProcessNG% = 11559        '工程履歴処理NG
194 MIN_PiasProcessOtherErr% = 11560  '工程履歴処理エラー(なんかのトラブル)
195 MIN_PiasProcessOK% = 11558        '工程履歴処理OK
196 '
197 MIN_Insight_Use% = 11369               '画像確認ON
198 MIN_TorqueCheck% = 11348               'トルクチェック
199 '
200 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT操作権
201 MOUT_RED_LIGHT% = 12356            'PATLIGHT 赤 点灯
202 MOUT_RED_FLASH% = 12357            'PATLIGHT 赤 点滅
203 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT 黄 点灯
204 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT 黄 点滅
205 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT 青 点灯
206 MOUT_GREEN_FLASH% = 12361          'PATLIGHT 青 点滅
207 '
208 MOUT_ST_DATETIME% = 12551          '組立開始日付時刻
209 MOUT_ED_DATETIME% = 12552          '組立終了日付時刻
210 '
211 MOUT_TORQUE_CHECK% = 12367         'PLCへトルクチェック中を送信
212 '
213 MIN_ASSY_CANCEL% = 11366           '組立を行うかのフラグ
214 '
215 MLoopFlg% = 0                      'KEY入力後のOK or NG内容
216 MRtn% = 0
217 MopeNo = 0
218 MRet = 0
219 'MRtn = 0
220 MRet3% = 0
221 '
222 Def Inte MInputQty          '投入数 演算変数
223 Def Inte MAssyOkQty         '組立ＯＫ数 演算変数
224 Def Inte MAssyNgQty         '組立ＮＧ数 演算変数(未使用)
225 Def Inte MSuctionErrQty     '吸着エラー数 演算変数 2022/04/27 渡辺
226 Def Inte nAssyOkQty         '未使用
227 Def Inte MScrewNo
228 Def Inte MReTry
229 '===== <IO変数定義> =====
230 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
231 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
232 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
233 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
234 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
235 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
236 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
237 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
238 '
239 Def Inte Y68_VV1            ' アーム先端　ネジ吸着バルブ
240 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
241 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
242 '
243 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
244 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
245 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
246 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
247 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
248 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
249 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
250 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
251 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
252 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
253 '
254 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
255 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
256 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
257 '
258 Def Inte MOUT_LED1          ' 画像処理用LED照明
259 '
260 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
261 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
262 '
263 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
264 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
265 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
266 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
267 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
268 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
269 '
270 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
271 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
272 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
273 '
274 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
275 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
276 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
277 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
278 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
279 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
280 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
281 Y68_VV1%    =  12248    ' アーム先端　ネジ吸着バルブ 'Y68_VV1% = 12250をY68_VV1% = 12248に変更(8/27中村)
282 Y6B_VB1%    =  12250    'アーム先端　吸着破壊バルブ'Y6B_VB1% = 12251をY6B_VB1% = 12250に変更(8/27中村)
283 MOUT_VB1%   =  12251    ' アーム先端　ネジ吸着破壊バルブ
284 '
285 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
286 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
287 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
288 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
289 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
290 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
291 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
292 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
293 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
294 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
295 '
296 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
297 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
298 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
299 '
300 MOUT_LED1%  =  12239    ' 画像処理用LED照明
301 '
302 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
303 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
304 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
305 '
306 '
307 '共通
308 Def Inte MTEST_KEY                      'デバックテスト用
309 Def Inte MOn                            '出力=1
310 Def Inte MOff                           '出力=0
311 '
312 'ねじ締め装置_出力アドレス
313 Def Inte MOUT_ScwT_ComChk               '通信確認
314 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
315 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
316 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
317 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
318 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
319 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
320 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
321 'ねじ締め装置_入力アドレス
322 Def Inte MIN_ScwT_comOK                 '通信確認返信
323 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
324 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
325 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
326 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
327 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
328 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
329 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
330 '
331 Def Inte MRetryLimit                    ' リトライ回数
332 Def Inte MRetryCount                    ' リトライカウント
333 '
334 Dim MScwT_Case1%(2)               '条件1停止変数
335 Dim MScwT_Case2%(2)               '条件2停止変数
336 Dim MScwT_Case3%(2)               '条件3停止変数
337 Dim MScwT_Case4%(2)               '条件4停止変数
338 Dim MScwT_Case5%(2)               '条件5停止変数
339 '
340 Def Pos PActive                     '直交座標系 位置変数 現在位置
341 Def Pos Pmove                       '直交座標系 位置変数 移動先
342 Def Inte MRecoveryPass              '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行'
343 '共通
344 MTEST_KEY% = 11359                       'デバッグ用テストKEY
345 MOn% = 1                                 '出力 = 1
346 MOff% = 0                                '出力 = 0
347 '
348 'ねじ締め機_アドレス設定
349 MOUT_ScwT_ComChk% = 12816               '通信確認送信
350 MOUT_ScwT_ST% = 12849                   'ねじ締め開始を送信
351 MOUT_ScwT_ReSTOK% = 12850               '再開始受信を送信
352 MOUT_ScwT_FinOK% = 12852                'ねじ締め完了受信を送信
353 MOUT_ScwT_Case1OK% = 12858              '条件1停止受信を送信
354 MOUT_ScwT_Case2OK% = 12859              '条件2停止受信を送信
355 MOUT_ScwT_Case3OK% = 12860              '条件3停止受信を送信
356 MOUT_ScwT_Case4OK% = 12861              '条件4停止受信を送信
357 MOUT_ScwT_Case5OK% = 12862              '条件5停止受信を送信
358 '
359 MIN_ScwT_comOK% = 11824                 'ねじ締め装置から返信
360 MIN_ScwT_STRec% = 11857                 'ねじ締め開始を受信
361 MIN_ScwT_ReST% = 11858                  '再開始を受信
362 MIN_ScwT_Fin% = 11860                   'ねじ締め完了を受信
363 MIN_ScwT_Case1% = 11866                 '条件1停止待機を受信
364 MIN_ScwT_Case2% = 11867                 '条件2停止待機を受信
365 MIN_ScwT_Case3% = 11868                 '条件3停止待機を受信
366 MIN_ScwT_Case4% = 11869                 '条件4停止待機を受信
367 MIN_ScwT_Case5% = 11870                 '条件5停止待機を受信
368 '
369 MScwT_Case1%(1) = MIN_ScwT_Case1%
370 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
371 MScwT_Case2%(1) = MIN_ScwT_Case2%
372 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
373 MScwT_Case3%(1) = MIN_ScwT_Case3%
374 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
375 MScwT_Case4%(1) = MIN_ScwT_Case4%
376 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
377 MScwT_Case5%(1) = MIN_ScwT_Case5%
378 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
379 '
380 '
381 PCalcGetMainScrew = (+0.00,+0.00,-1.20,+0.00,+0.00,+0.00,+0.00)  'Mainねじ供給機の補正値
382 PCalcGetFanScrew = (+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)   'Fanねじ供給機の補正値
383 '
384 MRetryLimit% = 2
385 '
386 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
387 Function M% fnAssyStart
388     M_20# = MClear%                       '初期化
389     '組み立て開始
390     'プログラム原点
391     Ovrd 100
392     ' 初期位置をIDチケット上とするため削除 9/16 M.Hayakawa
393 '    Mov PInitialPosition        '原点回避
394     '初期位置を設定
395     PTemp = P_Curr
396     MRtn = 0
397     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
398         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
399             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
400                 MRtn = 1
401                 Break
402             EndIf
403             Break
404         EndIf
405         Break
406     EndIf
407     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
408     If MRtn = 1 Then
409         M_Out(12256) = 1 Dly 0.3            '位置決め出ON
410         Mov PTicketRead
411         Break
412     Else
413         Mov PInitialPosition
414         M_Out(12256) = 1 Dly 0.3           '位置決め出ON
415         Mov PTicketRead_1           'チケットID読み取り回避点
416         Mvs PTicketRead             'ID読み位置
417         Break
418     EndIf
419     *RE_PUSH
420 '    If M_20# = MContinue% Then M_Out(12257) = 0
421     If M_20# = MContinue% Then M_Out(12256) = 1 Dly 0.3
422     If M_20# = MContinue% Then M_20# = MClear%
423     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '位置決め出端検出(8/26中村)
424     If MRtn = 1 Then GoTo *CompPush
425         M_Out(12257) = 1 Dly 0.3    ' Y71 1:位置決めCY 解除
426         fErrorProcess(11,231,282,0)
427     If M_20# = MNext% Then M_Out(12256) = 1 Dly 0.3 'Y70 1:位置決めCY 固定
428     If M_20# = MNext% Then M_20# = MClear%
429     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
430     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
431     If M_20# = MContinue% Then GoTo *RE_PUSH
432     *CompPush
433 '
434     *RE_READ
435     If M_20# = MContinue% Then M_20# = MClear%
436 '
437     MRtn = 1                            'MRtn初期化
438     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
439         MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
440     EndIf
441         '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
442         '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
443 '
444     If MRtn = 1 Then GoTo *CompRead
445     'fErrorProcess(11,97,25,0)
446 '    If M_20# = MPass% Then GoTo *ASSY_ERROR_END
447     If M_20# = MNext% Then M_20# = MClear%
448 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
449 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
450     If M_20# = MContinue% Then GoTo *RE_READ
451 '    If M_20# = MNext% Then M_20# = MPass%
452     GoTo *ASSY_ERROR_END
453     *CompRead
454     '
455 '【MAIN基板ID読み込み】
456     *RE_MEIN_CHECK
457     PInspPosition(1) = PMainPcbRead 'MAIN基板読込位置
458     MInspGroup%(1) = 2              '検査G番号
459     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
460 '
461     If MRtn = 1 Then GoTo *CompMainCheck
462     fErrorProcess(11,38,25,0)
463     If M_20# = MNext% Then M_20# = MClear%
464     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
465     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
466     If M_20# = MContinue% Then GoTo *RE_MEIN_CHECK
467     *CompMainCheck
468 '【GYRO基板ID読み込み】
469     *RE_GYRO_CHECK
470     PInspPosition(1) = PGyroPcbRead 'GYRO基板読込位置
471     MInspGroup%(1) = 3              '検査G番号
472     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
473 '
474     If MRtn = 1 Then GoTo *CompGyroCheck
475     fErrorProcess(11,38,25,0)
476     If M_20# = MNext% Then M_20# = MClear%
477     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
478     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
479     If M_20# = MContinue% Then GoTo *RE_GYRO_CHECK
480     *CompGyroCheck
481 '【基板IDコピー】
482     *RE_PCB_RECORD
483     M_Out(12571) = 1    ' 領域1 基板番号コピー (D2600-) On
484     Dly 0.1
485     M_Out(12572) = 1    ' 領域2 基板番号コピー (D2612-) On
486     Dly 0.1
487     M_Out(12566) = 1    ' toPLC_基板番号コピー要求 On
488 '
489     MRtn = frInCheck(11581,1,MSETTIMEOUT05&)    ' toRBT_基板番号コピー完了 On
490     If MRtn = 1 Then
491         M_Out(12571) = 0  ' 領域1 基板番号コピー (D2600-) Off
492         Dly 0.1
493         M_Out(12572) = 0  ' 領域2 基板番号コピー (D2612-) Off
494         Dly 0.1
495         M_Out(12566) = 0  ' toPLC_基板番号コピー要求 Off
496 '        GoTo *RE_PCB_COMPAIRE   ' 基板番号照合にスキップ
497     Else
498         fErrorProcess(11,39,25,0)
499         If M_20# = MNext% Then M_20# = MClear%
500         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
501         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
502         If M_20# = MContinue% Then GoTo *RE_PCB_RECORD
503     EndIf
504 '【基板ID照合（紐付け）】
505     MRetryCount% = 0
506     While (MRetryCount% <= MRetryLimit%)
507         *RE_PCB_COMPAIRE
508         M_Out(12557)= 1 ' 基板番号照合ビットON
509         MRtn = frInCheck(11566,1,MSETTIMEOUT05&)    ' toRBT_基板番号照合OK(M420) On
510         If MRtn = 1 Then
511             M_Out(12557)= 0     ' 基板番号照合ビットOff
512             ' リトライ回数設定でループを抜ける
513             MRetryCount% = 99
514         Else
515             If MRetryCount% = MRetryLimit% Then
516                 If M_In(11565) = 1 Then
517                     fErrorProcess(11,37,25,0)
518                 Else
519                     fErrorProcess(11,38,25,0)
520                 EndIf
521                 If M_20# = MNext% Then
522                     M_20# = MClear%
523                     ' リトライ回数設定でループを抜ける
524                     MRetryCount% = 99
525                 EndIf
526                 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
527                 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
528                 If M_20# = MContinue% Then
529                     MRetryCount% = 0
530                 EndIf
531             Else
532                 ' リトライ回数インクリメント
533                 MRetryCount% = MRetryCount% + 1
534                 Dly 0.1  ' 他の工程とタイミングをずらす為のディレイ
535             EndIf
536         EndIf
537     WEnd
538 '
539     *RE_CHECK
540     PInspPosition(1) = PParts1Check '部品1画像チェック位置(MAIN基板周辺）
541     MInspGroup%(1) = 4              '検査G番号
542     PInspPosition(2) = PParts2Check '部品2画像チェック位置（背面板周辺）
543     MInspGroup%(2) = 5              '検査G番号
544 '    PInspPosition(3) = PParts3Check '部品3画像チェック位置（SOC基板周辺）
545 '    MInspGroup%(3) = 6              '検査G番号
546 '    PInspPosition(4) = PParts4Check '部品4画像チェック位置（側板周辺）
547 '    MInspGroup%(4) = 7              '検査G番号
548     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 2, -1, 1 )  '画像処理検査実行
549     If MRtn = 1 Then GoTo *CompCheck
550     fErrorProcess(11,43,23,0)
551     If M_20# = MNext% Then M_20# = MClear%
552     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
553     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
554     If M_20# = MContinue% Then GoTo *RE_CHECK
555     *CompCheck
556     '
557     '製品位置決め(ID読込後に変更 9/16 M.Hayakawa）
558     *RE_POS
559     If M_20# = MContinue% Then M_20# = MClear%
560     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
561     M_Out(12256)=1 Dly 0.3      '位置決めCY用SV出端パルス出力
562     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
563 '
564     'Wait M_In(11266)=1          '位置決め出端検出修正につきコメントアウト(8/26中村))
565     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '位置決め出端検出(8/26中村)
566     If MRtn = 1 Then GoTo *Comp_Pos_1
567     fErrorProcess(11,231,282,0)
568     If M_20# = MNext% Then M_20# = MClear%
569     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
570     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
571     If M_20# = MContinue% Then GoTo *RE_POS
572     *Comp_Pos_1
573     '
574     M_Out(12258)=1 Dly 0.3      'プッシュCY用SV出端パルス出力(タクト短縮のため位置移動(12/13中村))
575     M_Out(12260)=1 Dly 0.3      'FANクランプ戻端パルス出力(タクト短縮のため位置移動(12/13中村))
576     Mov PScrewSupplyMain_1
577 '
578 '    M_Out(12258)=1 Dly 0.3      'プッシュCY用SV出端パルス出力
579     'Wait M_In(11268)=1          'プッシュ出端検出(修正につきコメントアウト(8/26中村))
580     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'プッシュ出端検出
581     If MRtn = 1 Then GoTo *Comp_Pos_2
582     fErrorProcess(11,231,282,0)
583     If M_20# = MNext% Then M_20# = MClear%
584     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
585     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
586     If M_20# = MContinue% Then GoTo *RE_POS
587     *Comp_Pos_2
588     '
589 '    M_Out(12260)=1 Dly 0.3      'FANクランプ戻端パルス出力(メインねじ締め後に変更 M.Hayakawa)(タクト短縮のため位置移動(12/13中村))
590     'Wait M_In(11270)=1          'FANクランプ戻端検出(修正につきコメントアウト(8/26中村))
591     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    'FANクランプ戻端検出(8/26中村)
592     If MRtn = 1 Then GoTo *Comp_Pos_3
593     fErrorProcess(11,231,282,0)
594     If M_20# = MNext% Then M_20# = MClear%
595     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
596     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
597     If M_20# = MContinue% Then GoTo *RE_POS
598     *Comp_Pos_3
599     '
600     '
601     'Main基板のネジ締め
602     'Main基板用ネジ供給機へネジを取りに行く
603     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
604     '
605     '*ScrewSupplyMain           '一時コメントアウト(以下5行,8/5中村)
606 '    Mov PScrewSupplyMain_2      'ネジ供給機回避点
607 '    Mov PScrewSupplyMain_1      'ネジピックアップ上空
608 '    Mvs PScrewSupplyMain        'ネジピックアップ
609 '    Mvs PScrewSupplyMain_1      'ネジピックアップ上空
610 '    Mov PScrewSupplyMain_2      'ネジ供給機回避点
611     'Return                     '一時コメントアウト(8/4中村)
612     'ScrewPositionDebug_1()      'デバック用(別関数使用のためコメントアウト(8/26中村))
613     '
614     PGetScrewPosTemp(1) = PScrewSupplyMain_1   'ネジピックアップ上空を代入(8/26中村)
615     PGetScrewPosTemp(2) = PScrewSupplyMain_2   'ネジ供給回避点を代入(8/26中村)
616     PGetScrewPosTemp(9) = PScrewSupplyMain_9   'ネジ供給機上空ネジ捨て位置(10/6 M.H追加)
617     PGetScrewPosTemp(10) = PScrewSupplyMain    'ネジピックアップを代入(8/26中村)
618     '
619     *RE_SCREW_GET_1                                'リトライ用ラベル
620     If M_20# = MContinue% Then M_20# = MClear%
621     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
622     If M_20# = MClear% Then GoTo *Comp_Screw_1
623     If M_20# = MNext% Then M_20# = MClear%
624     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
625     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
626     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
627     *Comp_Screw_1
628     '
629     '①番ネジ締め
630 '    Mov PScrewMain1_1           '①上空(以下5行一時コメントアウト(8/26中村))
631 '    Ovrd 5
632 '    Mvs PScrewMain1             '①ネジ着座
633 '    Ovrd 10
634 '    Mvs PScrewMain1_1           '①上空
635     PScrewPosTemp(1) = PScrewMain1_1    'ネジ1締め開始位置上空を代入(8/26中村)
636     PScrewPosTemp(2) = PScrewMain1_0    'ネジ1締め開始位置を代入(8/26中村)
637     PScrewPosTemp(10) = PScrewMain1     'ネジ1締め終了位置を代入(8/26中村)
638     M_Out16(12672) = 1              'ネジ締め位置番号送信
639     MRtn = ScrewTight(PScrewPosTemp,1,10.0)    'ネジ1締めの実行(8/26中村)
640     M_Out16(12672) = 0              'ネジ締め位置番号クリア
641     If MRtn = 1 Then GoTo *CompScrew1
642     Mov PInitialPosition
643     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
644     MScrewErrorCord% = MScrewErrorCord% + 1
645     fErrorProcess(11,MScrewErrorCord%,52,0)
646 '    fErrorProcess(11,53,52,0)
647     If M_20# = MNext% Then M_20# = MClear%
648     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
649     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
650     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
651     *CompScrew1
652     '
653     'Main基板用ネジ供給機へネジを取りに行く
654     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
655     'ScrewPositionDebug_1()      'デバック用(別関数使用のためコメントアウト(8/26中村))
656     *RE_SCREW_GET_2                                'リトライ用ラベル
657     If M_20# = MContinue% Then M_20# = MClear%
658     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
659     If M_20# = MClear% Then GoTo *Comp_Screw_2
660     If M_20# = MNext% Then M_20# = MClear%
661     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
662     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
663     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
664     *Comp_Screw_2
665     '②番ネジ締め
666 '    Mov PScrewMain2_1           '②上空(以下5行一時コメントアウト(8/26中村))
667 '    Ovrd 5
668 '    Mvs PScrewMain2             '②ネジ着座
669 '    Ovrd 10
670 '    Mvs PScrewMain2_1           '②上空
671     PScrewPosTemp(1) = PScrewMain2_1    'ネジ2締め開始位置上空を代入(8/26中村)
672     PScrewPosTemp(2) = PScrewMain2_0    'ネジ2締め開始位置を代入(8/26中村)
673     PScrewPosTemp(10) = PScrewMain2     'ネジ1締め終了位置を代入(8/26中村)
674     M_Out16(12672) = 2              'ネジ締め位置番号送信
675     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        'ネジ締め2の実行(8/26中村)
676     M_Out16(12672) = 0              'ネジ締め位置番号クリア
677     If MRtn = 1 Then GoTo *CompScrew2
678     Mov PInitialPosition
679     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
680     MScrewErrorCord% = MScrewErrorCord% + 2
681     fErrorProcess(11,MScrewErrorCord%,52,0)
682 '    fErrorProcess(11,54,52,0)
683     If M_20# = MNext% Then M_20# = MClear%
684     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
685     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
686     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
687     *CompScrew2
688     '
689     'Main基板用ネジ供給機へネジを取りに行く
690     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
691     *RE_SCREW_GET_3                                'リトライ用ラベル
692     If M_20# = MContinue% Then M_20# = MClear%
693     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
694     If M_20# = MClear% Then GoTo *Comp_Screw_3
695     If M_20# = MNext% Then M_20# = MClear%
696     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
697     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
698     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
699     *Comp_Screw_3
700     '③番ネジ締め
701 '    Mov PScrewMain3_1           '③上空(以下5行一時コメントアウト(8/26中村))
702 '    Ovrd 5
703 '    Mvs PScrewMain3             '③ネジ着座
704 '    Ovrd 10
705 '    Mvs PScrewMain3_1           '③上空
706     PScrewPosTemp(1) = PScrewMain3_1    'ネジ3締め開始位置上空を代入(8/26中村)
707     PScrewPosTemp(2) = PScrewMain3_0    'ネジ3締め開始位置を代入(8/26中村)
708     PScrewPosTemp(10) = PScrewMain3     'ネジ3締め終了位置を代入(8/26中村)
709     M_Out16(12672) = 3              'ネジ締め位置番号送信
710     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        'ネジ締め3の実行(8/26中村)
711     M_Out16(12672) = 0              'ネジ締め位置番号クリア
712     If MRtn = 1 Then GoTo *CompScrew3
713     Mov PInitialPosition
714     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
715     MScrewErrorCord% = MScrewErrorCord% + 3
716     fErrorProcess(11,MScrewErrorCord%,52,0)
717 '    fErrorProcess(11,55,52,0)
718     If M_20# = MNext% Then M_20# = MClear%
719     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
720     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
721     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
722     *CompScrew3
723     '
724     'Main基板用ネジ供給機へネジを取りに行く
725     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
726     *RE_SCREW_GET_4                                'リトライ用ラベル
727     If M_20# = MContinue% Then M_20# = MClear%
728     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
729     If M_20# = MClear% Then GoTo *Comp_Screw_4
730     If M_20# = MNext% Then M_20# = MClear%
731     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
732     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
733     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
734     *Comp_Screw_4
735     '④番ネジ締め
736 '    Mov PScrewMain4_1           '④上空(以下5行一時コメントアウト(8/26中村))
737 '    Ovrd 5
738 '    Mvs PScrewMain4             '④ネジ着座
739 '    Ovrd 10
740 '    Mvs PScrewMain4_1           '④上空
741     PScrewPosTemp(1) = PScrewMain4_1    'ネジ4締め開始位置上空を代入(8/26中村)
742     PScrewPosTemp(2) = PScrewMain4_0    'ネジ4締め開始位置を代入(8/26中村)
743     PScrewPosTemp(10) = PScrewMain4     'ネジ4締め終了位置を代入(8/26中村)
744     M_Out16(12672) = 4              'ネジ締め位置番号送信
745     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        'ネジ締め4の実行(8/26中村)
746     M_Out16(12672) = 0              'ネジ締め位置番号クリア
747     If MRtn = 1 Then GoTo *CompScrew4
748     Mov PInitialPosition
749     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
750     MScrewErrorCord% = MScrewErrorCord% + 4
751     fErrorProcess(11,MScrewErrorCord%,52,0)
752 '    fErrorProcess(11,56,52,0)
753     If M_20# = MNext% Then M_20# = MClear%
754     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
755     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
756     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
757     *CompScrew4
758     '
759     'Main基板用ネジ供給機へネジを取りに行く
760     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
761     *RE_SCREW_GET_5                                'リトライ用ラベル
762     If M_20# = MContinue% Then M_20# = MClear%
763     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
764     If M_20# = MClear% Then GoTo *Comp_Screw_5
765     If M_20# = MNext% Then M_20# = MClear%
766     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
767     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
768     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
769     *Comp_Screw_5
770     '⑤番ネジ締め
771 '    Mov PScrewMain5_1           '⑤上空(以下5行一時コメントアウト(8/26中村))
772 '    Ovrd 5
773 '    Mvs PScrewMain5             '⑤ネジ着座
774 '    Ovrd 10
775 '    Mvs PScrewMain5_1           '⑤上空
776     PScrewPosTemp(1) = PScrewMain5_1    'ネジ5締め開始位置上空を代入(8/26中村)
777     PScrewPosTemp(2) = PScrewMain5_0    'ネジ5締め開始位置を代入(8/26中村)
778     PScrewPosTemp(10) = PScrewMain5     'ネジ5締め終了位置を代入(8/26中村)
779     M_Out16(12672) = 5              'ネジ締め位置番号送信
780     MRtn = ScrewTight(PScrewPosTemp,6,10.0)        'ネジ締め5の実行(8/26中村)
781     M_Out16(12672) = 0              'ネジ締め位置番号クリア
782     If MRtn = 1 Then GoTo *CompScrew5
783     Mov PInitialPosition
784     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
785     MScrewErrorCord% = MScrewErrorCord% + 5
786     fErrorProcess(11,MScrewErrorCord%,52,0)
787 '    fErrorProcess(11,57,52,0)
788     If M_20# = MNext% Then M_20# = MClear%
789     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
790     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
791     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
792     *CompScrew5
793     '
794     'Main基板用ネジ供給機へネジを取りに行く
795     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
796 '以下3行PP品にネジ穴がないため一時削除 9/16 M.Hayakawa
797     *RE_SCREW_GET_6                                'リトライ用ラベル
798     If M_20# = MContinue% Then M_20# = MClear%
799     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
800     If M_20# = MClear% Then GoTo *Comp_Screw_6
801     If M_20# = MNext% Then M_20# = MClear%
802     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
803     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
804     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
805     *Comp_Screw_6
806     '⑥番ネジ締め
807 '    Mov PScrewMain6_1           '⑥上空(以下5行一時コメントアウト(8/26中村))
808 '    Ovrd 5
809 '    Mvs PScrewMain6             '⑥ネジ着座
810 '    Ovrd 10
811 '    Mvs PScrewMain6_1           '⑥上空
812 '以下3行PP品にネジ穴がないため一時削除 9/16 M.Hayakawa
813     PScrewPosTemp(1) = PScrewMain6_1    'ネジ6締め開始位置上空を代入(8/26中村)
814     PScrewPosTemp(2) = PScrewMain6_0    'ネジ6締め開始位置を代入(8/26中村)
815     PScrewPosTemp(10) = PScrewMain6     'ネジ6締め終了位置を代入(8/26中村)
816     M_Out16(12672) = 6              'ネジ締め位置番号送信
817     MRtn = ScrewTight(PScrewPosTemp,6,10.0)        'ネジ締め6の実行(8/26中村)
818     M_Out16(12672) = 0              'ネジ締め位置番号クリア
819     If MRtn = 1 Then GoTo *CompScrew6
820     Mov PInitialPosition
821     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
822     MScrewErrorCord% = MScrewErrorCord% + 6
823     fErrorProcess(11,MScrewErrorCord%,52,0)
824 '    fErrorProcess(11,58,52,0)
825     If M_20# = MNext% Then M_20# = MClear%
826     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
827     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
828     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
829     *CompScrew6
830     '
831     'FAN用ネジ供給機へネジを取りに行く
832     'GoSub *ScrewSupplyFan      '一時コメントアウト(8/4中村)
833 ' ネジ位置指定前に取りにいっている？ 1行一時削除 9/16 M.Hayakawa
834 '    MRtn = ScrewGet(PGetScrewPosTemp)       'ネジを取りに行く(8/26中村)
835     '
836 '    *ScrewSupplyFan
837 '    Mov PScrewSupplyFan_2       'ネジ供給機回避点
838 '    Mov PScrewSupplyFan_1       'ネジピックアップ上空
839 '    Mvs PScrewSupplyFan         ''ネジピックアップ
840 '    Mvs PScrewSupplyFan_1       'ネジピックアップ上空
841 '    Mov PScrewSupplyFan_2       'ネジ供給機回避点
842    ' Return                     '一時コメントアウト(8/4中村)
843     'ScrewPositionDebug_2()       'デバック用(別関数使用のためコメントアウト(8/26中村))
844     '
845     PGetScrewPosTemp(1) = PScrewSupplyFan_1   'ネジピックアップ上空を代入(8/26中村)
846     PGetScrewPosTemp(2) = PScrewSupplyFan_2   'ネジ供給回避点を代入(8/26中村)
847     PGetScrewPosTemp(9) = PScrewSupplyFan_9   'ネジ供給機上空ネジ捨て位置(10/6 M.H追加)
848     PGetScrewPosTemp(10) = PScrewSupplyFan    'ネジピックアップを代入(8/26中村)
849 '
850     *RE_SCREW_GET_7                                'リトライ用ラベル
851 '
852     If M_20# = MContinue% Then M_20# = MClear%
853     ScrewGet(PGetScrewPosTemp , 11260 , 0)          'ネジ受け取り開始
854     If M_20# = MClear% Then GoTo *Comp_Screw_7
855     If M_20# = MNext% Then M_20# = MClear%
856     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
857     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
858     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_7
859     *Comp_Screw_7
860     '⑦番ネジ締め
861 '    Mov PScrewFan1_1            '⑦上空(以下5行一時コメントアウト(8/26中村))
862 '    Ovrd 5
863 '    Mvs PScrewFan1              '⑦ネジ着座
864 '    Ovrd 10
865 '    Mvs PScrewFan1_1            '⑦上空
866     PScrewPosTemp(1) = PScrewFan1_1    'Fan1ネジ締め開始位置上空を代入(8/26中村)
867     PScrewPosTemp(2) = PScrewFan1_0    'Fan1ネジ締め開始位置を代入(8/26中村)
868     PScrewPosTemp(10) = PScrewFan1     'Fan1ネジ締め終了位置を代入(8/26中村)
869     M_Out16(12672) = 7              'ネジ締め位置番号送信
870     MRtn = ScrewTight(PScrewPosTemp,2,6.7)       'Fanネジ締め1の実行(8/26中村)
871     M_Out16(12672) = 0              'ネジ締め位置番号クリア
872     If MRtn = 1 Then GoTo *CompScrew7
873     Mov PInitialPosition
874     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
875     MScrewErrorCord% = MScrewErrorCord% + 7
876     fErrorProcess(11,MScrewErrorCord%,52,0)
877 '    fErrorProcess(11,59,52,0)
878     If M_20# = MNext% Then M_20# = MClear%
879     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
880     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
881     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_7
882     *CompScrew7
883 '
884     '
885     'FAN用ネジ供給機へネジを取りに行く
886     'GoSub *ScrewSupplyFan      '一時コメントアウト(8/4中村)
887     'ScrewPositionDebug_2()      'デバック用(別関数使用のためコメントアウト(8/26中村))
888     *RE_SCREW_GET_8                                'リトライ用ラベル
889     If M_20# = MContinue% Then M_20# = MClear%
890     ScrewGet(PGetScrewPosTemp , 11260 , 0)          'ネジ受け取り開始
891     If M_20# = MClear% Then GoTo *Comp_Screw_8
892     If M_20# = MNext% Then M_20# = MClear%
893     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
894     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
895     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_8
896     *Comp_Screw_8
897     '⑧番ネジ締め
898 '    Mov PScrewFan2_1            '⑧上空(以下5行一時コメントアウト(8/26中村))
899 '    Ovrd 5
900 '    Mvs PScrewFan2              '⑧ネジ着座
901 '    Ovrd 10
902 '    Mvs PScrewFan2_1            '⑧上空
903     PScrewPosTemp(1) = PScrewFan2_1    'Fan2ネジ締め開始位置上空を代入(8/26中村)
904     PScrewPosTemp(2) = PScrewFan2_0    'Fan2ネジ締め開始位置を代入(8/26中村)
905     PScrewPosTemp(10) = PScrewFan2     'Fan2ネジ締め終了位置を代入(8/26中村)
906     M_Out16(12672) = 8              'ネジ締め位置番号送信
907     MRtn = ScrewTight(PScrewPosTemp,2,6.7)       'Fanネジ締め2の実行(8/26中村)
908     M_Out16(12672) = 0              'ネジ締め位置番号クリア
909     If MRtn = 1 Then GoTo *CompScrew8
910     Mov PInitialPosition
911     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
912     MScrewErrorCord% = MScrewErrorCord% + 8
913     fErrorProcess(11,MScrewErrorCord%,52,0)
914 '    fErrorProcess(11,60,52,0)
915     If M_20# = MNext% Then M_20# = MClear%
916     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
917     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
918     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_8
919     *CompScrew8
920 '
921     'プログラム原点
922     'Mov PInitialPosition        ' 原点回避
923     MRtn = FnCtlValue2(2)       '組立ＯＫ＋１  2022/04/28 渡辺
924     Mov PTicketRead_1           ' チケットリード位置
925     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
926     InitialState()              ' 初期状態にする
927     M_20# = MAssyOK%              ' 正常終了処理
928     GoTo *fnAssyStart_FEndPosi
929 '
930 *ASSY_ERROR_END
931     fnInitialZone()   ' 初期位置に移動
932     InitialState()  ' 初期状態にする
933 *AssyEnd
934 *fnAssyStart_FEndPosi
935     Exit Function
936 FEnd
937 '
938 '■fnPiasCheck
939 ''' <summary>
940 ''' PIASチケット読込み
941 ''' </summary>
942 ''' <returns>   0 : NG
943 '''             1 : OK(読込み完了)
944 ''' </returns>
945 ''' <remarks>
946 ''' Date   : 2021/07/07 : M.Hayakawa
947 ''' </remarks>'
948 ''' <Update>
949 ''' Date   : 2022/01/11 : 中村
950 ''' </Update>
951 Function M% fnPiasCheck
952     fnPiasCheck = 0
953     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
954     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功
955 '
956 *RETRY_PIAS
957     M_20# = MClear%
958     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
959     '
960     '【IDチケット読み込み】
961     PInspPosition(1) = PTicketRead  'IDチケット読取位置
962     MInspGroup%(1) = 1              '検査G番号
963     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
964 '
965     'エラーの場合
966     If MRtn <> 1 Then
967         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
968         If MRtn <> 1 Then
969             'D720 -> D1300 コピー要求
970             M_Out(12565) = 1
971             Dly 0.5
972             M_Out(12565) = 0
973             'エラー処理記述
974             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
975             'GOT KEY入力待ち
976             MKeyNumber = fnKEY_WAIT()
977             '
978             Select MKeyNumber
979                 Case MNext%         '次へを選択した場合
980                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
981                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
982                     GoTo *fnPiasCheck_End                   'PIASチェック終了
983                     Break
984                 Case MAbout%        '停止を選択した場合
985                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
986                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
987                     GoTo *fnPiasCheck_End                   'PIASチェック終了
988                     Break
989                 Case MNgProcess%    'NGを選択した場合
990                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
991                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
992                     GoTo *fnPiasCheck_End                   'PIASチェック終了
993                     Break
994                 Case MContinue%     '継続を選択した場合
995                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
996                     M_20# = MContinue%
997                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
998                     Break
999             End Select
1000         EndIf
1001     EndIf
1002     If M_20# = MPass% Then GoTo *fnPiasCheck_End            'PIASチェック終了
1003     If M_20# = MAbout% Then GoTo *fnPiasCheck_End           'PIASチェック終了
1004     If M_20# = MNgProcess% Then GoTo *fnPiasCheck_End       'PIASチェック終了
1005     If M_20# = MContinue% Then GoTo *RETRY_PIAS             'PIASチェックリトライ
1006 '----------D720 -> D1300 コピー要求----------
1007     M_Out(12565) = 1
1008     Dly 0.5
1009     M_Out(12565) = 0
1010 '----------通信確認をする----------
1011     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1012     MRtn = 0                ' 初期化
1013     M_20# = MClear%         ' 初期化
1014     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1015     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1016 '    If MRtn <> 1 Then
1017 '        If M_20# = MContinue% Then
1018 '            GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1019 '        Else
1020 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1021 '        EndIf
1022 '    EndIf
1023     If MRtn = 1 Then GoTo *PCComu_OK                '通信OK時ラベルへジャンプ
1024     If M_20# = MContinue% Then GoTo *RETRY_PIAS      'チケット読み直しからリトライ
1025     GoTo *fnPiasCheck_End                           'その他の場合PIASチェック終了
1026     *PCComu_OK
1027 '----------工程抜け確認----------
1028     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1029     MRtn = 0                ' 初期化
1030     M_20# = MClear%         ' 初期化
1031     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1032     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1033 '    If MRtn <> 1 Then
1034 '        If M_20# = MContinue% Then
1035 '            GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1036 '        Else
1037 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1038 '        EndIf
1039 '    EndIf
1040     If MRtn = 1 Then GoTo *ProcessCheck_OK                '工程チェックOK時ラベルへジャンプ
1041     If M_20# = MContinue% Then GoTo *RETRY_PIAS      'チケット読み直しからリトライ
1042     GoTo *fnPiasCheck_End                           'その他の場合PIASチェック終了
1043     *ProcessCheck_OK
1044     '
1045     fnPiasCheck = 1
1046     *fnPiasCheck_End
1047     Exit Function
1048 FEnd
1049 '
1050 '■fnPCComuCheck
1051 ''' <summary>
1052 ''' PC-PLC通信チェック
1053 ''' </summary>
1054 ''' <returns>   0 : NG
1055 '''             1 : OK(読込み完了)
1056 ''' </returns>
1057 ''' <remarks>
1058 ''' Date   : 2021/07/07 : M.Hayakawa
1059 ''' </remarks>'
1060 Function M% fnPCComuCheck
1061     fnPCComuCheck = 0
1062     MJudge% = 0                                  '初期化
1063     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1064     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1065     '
1066     For MStaNo = 0 To 5
1067         '
1068         If M_In(MIN_PIAS_ComOK%) = 1 Then
1069             'PC通信OK(M400)
1070             MJudge% = MOK%
1071             MStaNo = 5
1072             Break
1073         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1074             'toRBT_通信確認time out
1075             MJudge% = MNG%
1076             MCommentD1001 = 15
1077             MCommentD1002 = 21
1078             MStaNo = 5
1079             Break
1080         Else
1081             'toRBT_通信確認time out
1082             MJudge% = MNG%
1083             MCommentD1001 = 14
1084             MCommentD1002 = 21
1085             Break
1086         EndIf
1087     Next MStaNo
1088     '
1089     '上記で返信フラグを受信してからPC通信確認OFF
1090     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1091     '
1092     'エラー画面
1093     If MJudge% <> MOK% Then
1094         M_20# = MClear%     '初期化
1095         'エラー処理記述
1096         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1097         'GOT KEY入力待ち
1098         MKeyNumber = fnKEY_WAIT()
1099         '
1100         If MKeyNumber = MAbout% Then            '停止を選択した場合
1101             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1102             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1103             Break
1104         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1105             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1106             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1107             Break
1108         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1109             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1110             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1111             Break
1112         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1113             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1114             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1115             Break
1116         EndIf
1117     Else
1118         'OKの場合
1119         fnPCComuCheck = 1
1120     EndIf
1121     Exit Function
1122 FEnd
1123 '
1124 '■fnProcessCheck
1125 ''' <summary>
1126 ''' 工程抜け確認
1127 ''' </summary>
1128 ''' <returns>    1：工程履歴OK     0：異常終了
1129 '''             -1：前工程履歴NG  -2：自工程履歴あり
1130 '''             -3：モデル仕向NG  -4：タイムアウト
1131 '''             -5：履歴処理エラー
1132 ''' </returns>
1133 ''' <remarks>
1134 ''' Date   : 2021/07/07 : M.Hayakawa
1135 ''' </remarks>'
1136 Function M% fnProcessCheck
1137     fnProcessCheck = 0
1138     MJudge% = MNG%      '一旦NGを初期化とする
1139 '----------工程抜け確認----------
1140     MCommentD1001 = 0   'コメント初期化
1141     For MStaNo = 0 To 5
1142         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1143         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1144         '
1145         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1146             MJudge% = MOK%
1147             fnAutoScreenComment(85)     ' AUTO画面
1148             MStaNo = 5
1149             Break
1150         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1151             MFlgLoop% = 0
1152             MJudge% = MNG%
1153             MCommentD1001 = 27
1154             MCommentD1002 = 22
1155             fnAutoScreenComment(94)     ' AUTO画面
1156             fnProcessCheck = -2         ' NGは-2を返す
1157             MStaNo = 5
1158             Break
1159         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1160            MJudge% = MNG%
1161             MCommentD1001 = 31
1162             MCommentD1002 = 22
1163             fnAutoScreenComment(83)     ' AUTO画面
1164             fnProcessCheck = -3         ' NGは-3を返す
1165             MStaNo = 5
1166             Break
1167         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1168             '履歴NGは直ぐに終了せず繰り返し確認を行う
1169             '前工程の書込みが終了していない可能性があるため
1170             MJudge% = MNG%
1171             MCommentD1001 = 32
1172             MCommentD1002 = 22
1173             fnAutoScreenComment(84)     ' AUTO画面
1174             fnProcessCheck = -1         ' NGは-1を返す
1175             Dly 1.0
1176             '工程抜け確認OFF
1177             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1178             Dly 1.0
1179            'MStaNo = 5
1180             Break
1181         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1182             MFlgLoop% = 0
1183             MJudge% = MNG%
1184             MCommentD1001 = 29
1185             MCommentD1002 = 22
1186             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1187             fnProcessCheck = -5         ' NGは-5を返す
1188             MStaNo = 5
1189             Break
1190         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1191             MJudge% = MNG%
1192             If MCommentD1001 = 32 Then
1193                 '何もしない
1194             Else
1195                 MCommentD1001 = 26
1196             EndIf
1197             MCommentD1002 = 22
1198             fnProcessCheck = -4         ' NGは-4を返す
1199             MStaNo = 5
1200             Break
1201         Else
1202             MJudge% = MNG%
1203             MCommentD1001 = 28
1204             MCommentD1002 = 22
1205         EndIf
1206     Next MStaNo
1207     '工程抜け確認OFF
1208     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1209     '通過履歴NG 工程抜けの場合
1210     If MJudge% = MPass% Then
1211         M_20# = MPass%
1212     EndIf
1213     '
1214     'エラー画面
1215     If MJudge% <> MOK% Then
1216         M_20# = MClear%     '初期化
1217         'エラー処理記述
1218         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1219         'GOT KEY入力待ち
1220         MKeyNumber = fnKEY_WAIT()
1221         '
1222         Select MKeyNumber
1223             Case MAbout%        '停止を選択した場合
1224                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1225                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1226                 Break
1227             Case MNext%         '次へを選択した場合
1228                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1229                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1230                 Break
1231             Case MContinue%     '継続を選択した場合
1232                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1233                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1234                 Break
1235             Case MNgProcess%    'NGを選択した場合
1236                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1237                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1238                 Break
1239         End Select
1240     Else
1241         fnProcessCheck = 1  ' OKは1を返す
1242     EndIf
1243     Exit Function
1244 FEnd
1245 '
1246 '■fnPiasWrite
1247 ''' <summary>
1248 ''' Pias 組立結果書込み要求
1249 ''' </summary>
1250 '''<param name="MFlg%">
1251 ''' MOK%(1) = 工程履歴にOKを書込む
1252 ''' MNG%(0) = 工程履歴にNGを書込む
1253 '''</param>
1254 '''<returns></returns>
1255 ''' <remarks>
1256 ''' Date   : 2021/07/07 : M.Hayakawa
1257 ''' </remarks>'
1258 Function M% fnPiasWrite(ByVal MFlg%)
1259       fnPiasWrite = 0
1260 *RETRY_PIASWRITE
1261     '
1262     '組立OK(MOK%)の場合　M306 ON
1263    '組立NG(MNG%)の場合　M307 ON
1264     If MFlg% = MOK% Then
1265         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1266     Else
1267         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1268     EndIf
1269     Dly 0.1                  '念のため
1270     '
1271     'Piasへ書込み開始 M305 -> ON
1272     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1273     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1274     '
1275     MJudge% = MNG%
1276     '
1277     For MStaNo = 0 To 5
1278         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1279             MJudge% = MOK%
1280             'MRet = fnAutoScreenComment(85)  'AUTO画面
1281             MStaNo = 5
1282             Break
1283         '
1284         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1285             MJudge% = MNG%
1286             'MRet = fnAutoScreenComment(85)  'AUTO画面
1287            MCommentD1001 = 34
1288            MCommentD1002 = 25
1289             MStaNo = 5
1290             Break
1291         '
1292         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1293             MJudge% = MNG%
1294             'MRet = fnAutoScreenComment(85)  'AUTO画面
1295            MCommentD1001 = 35
1296            MCommentD1002 = 25
1297             MStaNo = 5
1298             Break
1299         '
1300         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1301             MJudge% = MNG%
1302             'MRet = fnAutoScreenComment(85)  'AUTO画面
1303            MCommentD1001 = 36
1304            MCommentD1002 = 25
1305             MStaNo = 5
1306             Break
1307         '
1308         Else
1309             MJudge% = MNG%
1310            MCommentD1001 = 42
1311            MCommentD1002 = 25
1312         '
1313         EndIf
1314         '
1315     Next MStaNo
1316     '
1317     'Piasへ書込み開始 M305 -> OfF
1318     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1319     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1320     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1321     '
1322     '
1323     '通過履歴NG 工程抜けの場合
1324     If MJudge% = MPass% Then
1325         M_20# = MPass%
1326     EndIf
1327     '
1328    M_20# = MClear%     '初期化
1329     '
1330     'エラー画面
1331     If MJudge% < MOK% Then
1332     '
1333 '残しておくが現状では使用しないラベル
1334 *RETRY_ERR_WRITE
1335         M_20# = MClear%     '初期化
1336         'エラー処理記述
1337         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1338         'GOT KEY入力待ち
1339         MKeyNumber = fnKEY_WAIT()
1340         '
1341         If MKeyNumber = MAbout% Then   '停止を選択した場合
1342             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1343            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1344             Break
1345         '
1346         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1347             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1348             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1349             Break
1350         '
1351         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1352             M_20# = MPass%            'M_20# プログラム間共通外部変数
1353             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1354             Break
1355         '
1356         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1357             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1358            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1359             Break
1360         '
1361         EndIf
1362         '
1363 '        If M_20# = MClear% Then *RETRY_ERR_WRITE
1364         '
1365     EndIf
1366     '
1367     If M_20# = MContinue% Then *RETRY_PIASWRITE
1368     '
1369     fnPiasWrite = 1
1370     Exit Function
1371 FEnd
1372 '
1373 '■fnPCBNumberCheck
1374 ''' <summary>
1375 ''' Pias 基板番号照合要求
1376 ''' </summary>
1377 '''<returns>0（固定）</returns>
1378 ''' <remarks>
1379 ''' Date   : 2021/07/07 : M.Hayakawa
1380 ''' </remarks>'
1381 Function M% fnPCBNumberCheck
1382       fnPCBNumberCheck = 0
1383     '
1384 *RETRY_PCBCHECK
1385     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1386     'Piasへ基板照合開始 M310 -> ON
1387     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1388     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1389     '
1390     MJudge% = MNG%
1391     '
1392     For MStaNo = 0 To 5
1393         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1394             MJudge% = MOK%
1395             fnAutoScreenComment(96)  'AUTO画面
1396             MStaNo = 5
1397             Break
1398         '
1399         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1400             MJudge% = MNG%
1401             fnAutoScreenComment(97)  'AUTO画面
1402             MCommentD1001 = 37
1403             MCommentD1002 = 25
1404             MStaNo = 5
1405             Break
1406         '
1407         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1408             MJudge% = MNG%
1409             fnAutoScreenComment(98)  'AUTO画面
1410             MCommentD1001 = 38
1411             MCommentD1002 = 25
1412             MStaNo = 5
1413             Break
1414         '
1415         ElseIf M_In(11580) = 1 Then                         'time out
1416             MJudge% = MNG%
1417             fnAutoScreenComment(99)  'AUTO画面
1418             MCommentD1001 = 39
1419             MCommentD1002 = 25
1420             MStaNo = 5
1421             Break
1422         '
1423         Else
1424             MJudge% = MNG%
1425            MCommentD1001 = 41
1426            MCommentD1002 = 25
1427         '
1428         EndIf
1429         '
1430     Next MStaNo
1431     '
1432     'Piasへ基板照合開始 M310 -> OfF
1433     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1434     '
1435     '
1436     '通過履歴NG 工程抜けの場合
1437     If MJudge% = MPass% Then
1438         M_20# = MPass%
1439     EndIf
1440     '
1441    M_20# = MClear%     '初期化
1442     '
1443     'エラー画面
1444     If MJudge% < MOK% Then
1445     '
1446 '残しておくが現状では使用しないラベル
1447 *RETRY_ERR_PCBNUMBER
1448         M_20# = MClear%     '初期化
1449         'エラー処理記述
1450         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1451         'GOT KEY入力待ち
1452         MKeyNumber = fnKEY_WAIT()
1453         '
1454         If MKeyNumber = MAbout% Then   '停止を選択した場合
1455             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1456             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1457             Break
1458         '
1459         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1460             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1461             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1462         '
1463         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1464             M_20# = MPass%            'M_20# プログラム間共通外部変数
1465             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1466         '
1467         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1468             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1469             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1470             Break
1471         '
1472         EndIf
1473         '
1474 '        If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1475         '
1476     EndIf
1477     '
1478     If M_20# = MContinue% Then *RETRY_PCBCHECK
1479     Exit Function
1480 FEnd
1481 '
1482 '■ScrewTight
1483 ''' <summary>
1484 ''' ねじ締めを行う(Sタイト)
1485 ''' </summary>
1486 '''<param name="PScrewPos()">
1487 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1488 '''             PScrewPos(2)    ：ねじ締め回避点
1489 '''             PScrewPos(10)   ：ねじ締め終了高さ
1490 '''<param name="MScrewType">ネジタイプ(mm/sec)
1491 '''             1:6mm Sタイト銀ネジ
1492 '''             2:8mm Pタイト
1493 '''             3:6mm Sタイト黒ネジ
1494 '''             4:13mm Sタイト
1495 '''             5:6mm Mネジ
1496 '''</param>
1497 '''<param name="MFeedSpd">送り速度(mm/sec)</param>
1498 '''<returns>整数
1499 '''         0=異常終了、1=正常終了
1500 '''</returns>
1501 ''' <remarks>
1502 ''' Date   : 2021/07/07 : M.Hayakawa
1503 ''' Update : 2021/09/28 : M.Hayakawa ネジタイプ、送り速度を引数に追加
1504 ''' </remarks>'
1505 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   'ネジ締め個別設定
1506     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
1507     ScrewTight = 0
1508     MOKNGFlg = 0
1509     Ovrd 100
1510     Mov PScrewPosition(1)       ' パレット上ねじ締めS①の安全回避位置
1511     Fine 0.05 , P
1512     Ovrd MOvrdA%
1513     ' 減速設定
1514     Accel 100, 10
1515     ' パレット上ねじ締め開始位置へ移動
1516     Mvs PScrewPosition(2)
1517     ' 加減速を元に戻す
1518     Accel
1519     ' 内部Ovrd設定
1520 '    Ovrd MOvrdA%
1521     Ovrd 100
1522     ' Spd設定
1523 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
1524     Spd MFeedSpd
1525     ' 設定進み量5.0 × 操作パネルのオーバーライド係数 × プログラム内オーバーライド係数
1526     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
1527     Select MScrewType%
1528         Case 1
1529             ' Sタイト：プログラム1、バンク1に設定
1530             ProgramBankSet(1,1)
1531             Break
1532         Case 2
1533             ' Pタイト：プログラム1、バンク1に設定
1534             ProgramBankSet(3,1)
1535             Break
1536         Case 3
1537             ' Sタイト黒：プログラム1、バンク1に設定
1538             ProgramBankSet(1,1)
1539             Break
1540         Case 4
1541             ' Sタイト13mm：プログラム1、バンク1に設定
1542             ProgramBankSet(1,1)
1543             Break
1544         Case 5
1545             ' Mネジ：プログラム1、バンク1に設定
1546             ProgramBankSet(1,1)
1547             Break
1548         Case 6
1549             ' Sタイト：プログラム1、バンク4に設定
1550             ProgramBankSet(1,4)
1551             Break
1552         Default
1553             ' プログラム1、バンクなし設定
1554             ProgramBankSet(0,0)
1555             Break
1556     End Select
1557 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     'ドライバーON　CW
1558      'ドライバーON　CW
1559     M_Out(12241)=1
1560     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1561     Wait M_In(11584)=1          '完了/エラー検出 暫定コメント 10/6 M.H
1562     Dly 0.1
1563     Fine 0 , P
1564     Spd M_NSpd
1565     '
1566     If M_In(11256)=1 Then  'ねじトータルエラー検出時
1567         M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1568         Dly 0.1
1569        ' プログラム・バンク解除
1570         ProgramBankSet(0,0)
1571         'パレット上ねじ締め終了位置上空へ移動
1572         Mvs PScrewPosition(10),-80
1573         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1574         M_Out(12249)=1 Dly 0.3
1575         MOKNGFlg = -1
1576         ScrewTight = 0
1577     Else
1578          'ドライバーOFF　CW
1579         M_Out(12241)=0
1580 '        エラーがない場合はネジ締め終了位置で増し締め
1581 '        Select MScrewType%
1582 '            Case 1
1583 '                ' Sタイト：プログラム1、バンク3に設定
1584 '                ProgramBankSet(1,3)
1585 '                Break
1586 '            Case 2
1587 '                ' Pタイト：プログラム1、バンク3に設定
1588 '                ProgramBankSet(3,3)
1589 '                Break
1590 '            Case 3
1591 '                ' Sタイト黒：プログラム1、バンク3に設定
1592 '                ProgramBankSet(1,3)
1593 '                Break
1594 '            Case 4
1595 '                ' Sタイト13mm：プログラム1、バンク3に設定
1596 '                ProgramBankSet(1,3)
1597 '                Break
1598 '            Case 5
1599 '                ' Mネジ：プログラム1、バンク3に設定
1600 '                ProgramBankSet(1,3)
1601 '                Break
1602 '            Default
1603 '                ' プログラム1、バンクなし設定
1604 '                ProgramBankSet(0,0)
1605 '                Break
1606 '        End Select
1607 '         'ドライバーON　CW
1608 '        Mvs PScrewPosition(10)
1609 '        M_Out(12241)=1
1610 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1611 '
1612          'ドライバーOFF　CW
1613         M_Out(12241)=0
1614        ' プログラム・バンク解除
1615         ProgramBankSet(0,0)
1616         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1617         M_Out(12249)=1 Dly 0.3
1618     '     ↓PScrewPos(2) → PScrewPosition(10)に変更 9/16 M.Hayakawa
1619         'パレット上ねじ締め終了位置上空へ移動
1620        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1621         'Mvs PScrewPosition(10),-80
1622         ScrewTight = 1
1623     EndIf
1624 ' 暫定（暫定マスク　9/16 M.Hayakawa)
1625 '    Ovrd 10
1626 '    Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1627     Ovrd 100
1628     Exit Function
1629 FEnd
1630 '
1631 '■ScrewGet
1632 ''' <summary>
1633 ''' ねじ供給機からねじを得る
1634 ''' </summary>
1635 '''<param name="%">
1636 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1637 '''         PScrewPos(2)    ：ねじ供給器回避点
1638 '''         PScrewPos(9)    ：ねじ供給器上空ネジ捨位置
1639 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1640 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1641 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1642 '''</param>
1643 '''<param name = FeederReadyNo%> 指定の供給機Ready </param>
1644 '''<param name = FeederScrewSensor%> 指定の誤供給防止センサー指定(0でセンサー無し)</param>
1645 '''<returns>整数
1646 '''         0=異常終了、1=正常終了、-1=ねじ供給NG、-2=ねじ誤供給NG、-3=吸着エラー
1647 '''</returns>
1648 ''' <remarks>
1649 ''' Date   : 2021/07/07 : M.Hayakawa
1650 ''' </remarks>
1651 '''<update>
1652 '''Date    : 2021/11/15 : 中村
1653 '''Date    : 2021/02/07 : 早川 念のため確認を削除
1654 '''</update>
1655 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1656     fnAutoScreenComment(522)    '状態表示[ネジ供給待ち] 2022/05/09 渡辺
1657     ScrewGet = 0
1658     MScrewJudge% = 0
1659     MFinCnt% = 2
1660     'ねじ供給器初期動作エラーチェック
1661 ' ↓暫定削除
1662     'Mov PScrewPosition(2)   'ねじ供給機回避点へ移動
1663     For MCnt% = 0 To MFinCnt%
1664         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '指定ねじ供給機がReadyになっているか確認(5秒間)
1665         If MRtn = 0 Then
1666             M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1667             ScrewGet = -1
1668             MScrewJudge% = 2
1669         EndIf
1670         Ovrd 100
1671         If FeederScrewSensor% <> 0 Then
1672             If M_In(FeederScrewSensor%) = 1 Then  '誤供給が検出されたら
1673                 'Ovrd 30
1674                 M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1675                 'NGとしてここの関数から抜ける
1676                 ScrewGet = -2
1677                 MScrewJudge% = 3
1678             EndIf
1679         EndIf
1680         Ovrd 100
1681         Spd M_NSpd
1682         If MScrewJudge% = 0 Then
1683     '        ScrewGet = 0
1684             M_Out(Y63_Driver)=1         ' バンクセッティング　C2
1685 '            Dly 0.3
1686             MScrewCnt% = 0
1687             MFinCnt% = 2
1688             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
1689             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1690             'ビット回転(シーケンス見直しにつき処理位置変更4/3中村)
1691             M_Out(Y60_Driver)=1
1692             'ビット回転安定状態監視開始
1693             '
1694             '
1695             'Ovrd 40 '2に変更 10/6 M.H '5に変更10/7中村
1696             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1697             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
1698             'Mvs PScrewPosition(10), 1.2
1699            Mvs PScrewPosition(10)       'Fan用ねじ吸着位置修正のため変更 2022-02-01AJI
1700 '            'ビット回転(シーケンス見直しにつき処理位置変更4/3中村)
1701 '            M_Out(Y60_Driver)=1
1702 '            'ビット回転安定状態監視開始
1703             M_Timer(4) = 0
1704             MloopFlg = 0
1705             MCrtTime& = 0
1706            'ビット回転安定まで待機
1707             While MloopFlg = 0
1708                 MCrtTime& = M_Timer(4)
1709                 If MCrtTime& >= 180 Then
1710                     MloopFlg = 1
1711                 EndIf
1712             WEnd
1713             '
1714            M_Out(Y68_VV1)=1 Dly 0.3     ' ねじ吸着　ON'Dly 0.3追加(8/27中村)
1715             'ビット回転(シーケンス見直しにつき処理位置変更4/3中村)
1716 '            M_Out(Y60_Driver)=1
1717 '            Dly 0.2
1718             '吸着位置にて吸着確認
1719             MRtn = 0
1720             MRtn = frInCheck(11264, 1, MSETTIMEOUT01&)
1721             '
1722             JOvrd M_NJovrd
1723             Spd M_NSpd
1724             'ネジ吸着確認位置移動
1725             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
1726             Mvs PScrewPosition(10), -30  ' ネジ吸着確認位置
1727            'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1728             'ビット回転停止
1729             M_Out(Y60_Driver)=0
1730             '
1731 '            If MRtn = 1 Then            'シーケンス変更につきコメントアウト(5/13中村)
1732                 '1秒間ネジ吸着確認 始めの閾値
1733                 MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1734 '            EndIf                       'シーケンス変更につきコメントアウト(5/13中村)
1735             'MRtn = 0'強制エラー
1736             '吸着エラーの場合
1737             'ネジをねじ太郎に戻す
1738             If MRtn = 0 Then
1739                 Ovrd 5      '2から5に変更
1740                 'ビット回転停止
1741                 M_Out(Y60_Driver)=0
1742                 'ネジ供給機上空
1743                 Mvs PScrewPosition(1)
1744                 '更に上空
1745                 Mov PScrewPosition(1), -140
1746                 'ネジ捨て位置
1747                 If FeederReadyNo% = 11260 Then     '供給機別に吸着エラー数をカウント　2022/05/19 渡辺
1748                     MRtn = FnCtlValue2(3)          '供給機２吸着エラー数＋１
1749                 Else
1750                     MRtn = FnCtlValue2(4)          '供給機１吸着エラー数＋１  2022/04/28 渡辺
1751                 EndIf
1752                 Mov PScrewPosition(9)
1753                 MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1754                 '吸着OFF
1755                 M_Out(12249)=1 Dly 0.3 'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1756                 Dly 0.2
1757                 '破壊ON
1758                 M_Out(Y6B_VB1)=1 '真空破壊ON
1759                 'ビット回転
1760                 M_Out(Y61_Driver)=1
1761                 Dly 0.5
1762                 '                '
1763                 Ovrd 100
1764                 JOvrd M_NJovrd
1765                 Spd M_NSpd
1766                 'ドライバーを上下させねじを振り落とす
1767                 Mov PScrewPosition(9), 10
1768                 Mov PScrewPosition(9)
1769                 Dly 0.1
1770                 Mov PScrewPosition(9), 10
1771                 Mov PScrewPosition(9)
1772                 '
1773                 'ネジ落ち待ち
1774                 Wait M_In(11265) = 0
1775                 'ビット回転停止
1776                 M_Out(Y61_Driver)=0
1777                 Dly 0.1
1778                 '破壊OFF
1779                 M_Out(Y6B_VB1)=0 '真空破壊OFF
1780                 'ねじ落ちたとして、移動更に上空
1781                 Mov PScrewPosition(1), -140
1782                 Ovrd 100
1783                 Spd M_NSpd
1784                 'ネジ供給機上空
1785                 Mvs PScrewPosition(1)
1786 '                '
1787                 ScrewGet = -3
1788                 If MCnt% = MFinCnt% Then
1789                     MScrewJudge% = 4
1790                     Mov PScrewPosition(2)
1791                     Break
1792                 EndIf
1793                 Break
1794 '                '
1795             Else
1796                 MCnt% = MFinCnt%
1797                 ScrewGet = 1
1798             EndIf
1799         Else
1800             MCnt% =MFinCnt%
1801         EndIf
1802     Next  MCnt%
1803         '
1804 '    If MScrewJudge% = 0 Then
1805 '        Ovrd 100
1806 '        Spd M_NSpd
1807 '        PScrewPosition(1)
1808 '        Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1809 '        'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1810 '        M_Out(Y60_Driver)=0     ' ビット回転停止
1811 '        M_Out(Y63_Driver)=0     ' バンクセッティング　C2
1812 '        'Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1813 '        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1814 '        'Mov PScrewPosition(2)
1815 '        'もう一度吸着確認　上空の最終閾値
1816 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1817 '        If MRtn = 0 Then      '吸着エラーの場合
1818 '            MScrewJudge% = 4
1819 '            ScrewGet = -3
1820 '        ElseIf MRtn = 1 Then      '吸着OKの場合
1821 '            MScrewJudge% = 1
1822 '            ScrewGet = 1
1823 '        EndIf
1824 '        Break
1825 '    EndIf
1826     '
1827 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1828     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1829     '
1830     Select MScrewJudge%
1831 '        Case 0
1832 ''            fErrorProcess(11,162,163,0) '異常終了
1833 '            MCommentD1001 = 162
1834 '            MCommentD1002 = 96
1835 '            Break
1836         Case 2
1837 '            fErrorProcess(11,63,161,0) '供給NG
1838             MCommentD1001 = 63
1839             MCommentD1002 = 96
1840             Break
1841         Case 3
1842 '            fErrorProcess(11,160,164,0) '誤供給
1843             MCommentD1001 = 237
1844             MCommentD1002 = 96
1845             Break
1846         Case 4
1847 '            fErrorProcess(11,94,95,0) '吸着NG
1848             MCommentD1001 = 94
1849             MCommentD1002 = 95
1850             Break
1851     End Select
1852     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1853     '
1854     Select M_20#
1855         Case MAbout%          '停止が押された場合
1856             Mov PScrewPosition(2)                  '初期位置に戻って停止処理
1857             Mov PInitialPosition
1858             Break
1859         Case MContinue%       'リトライが押されていた場合(関数を抜けた先で処理)
1860             Break
1861         Case MNext%           '継続が押された場合
1862             M_20# = MClear%     '初期化
1863             Break
1864         Case MNgProcess%      'NGが押された場合
1865             Mov PScrewPosition(2)   'PIASにNG書き込みを行い,初期位置に戻って行程終了
1866             Mov PInitialPosition
1867             Break
1868         End Select
1869 *End_ScrewGet
1870     Exit Function
1871 FEnd
1872 '
1873 '■ProgramBankSet
1874 ''' <summary>
1875 ''' ねじ締めを行う(Pタイト)
1876 ''' </summary>
1877 '''<param name="MProgramNo">プログラム番号</param>
1878 '''<param name="MBankNo">バンク番号</param>
1879 '''</returns>
1880 ''' <remarks>
1881 ''' Date   : 2021/10/05 : M.Hayakawa
1882 ''' </remarks>'
1883 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1884 '
1885     MLocalPrgNo% = (MProgramNo% - 1) * 32
1886     MLocalBankNo% = MBankNo% * 4
1887 '
1888     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1889         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1890     Else
1891         MLocalOutNo% = 0
1892     EndIf
1893 '
1894     M_Out8(12240) = MLocalOutNo%
1895     Dly 0.1
1896     Exit Function
1897 FEnd
1898 '
1899 '■fnKEY_WAIT()
1900 ''' <summary>
1901 ''' GOTからのキー入力待ち
1902 ''' </summary>
1903 '''<returns>1：停止    2：次へ
1904 '''         3：継続    4：トルクチェック開始
1905 '''         5：NG
1906 '''         11：ロボット初期位置1    12：ロボット初期位置2
1907 '''         13：ロボット初期位置3    14：ロボット初期位置4
1908 '''</returns>
1909 ''' <remarks>
1910 ''' Date   : 2021/07/07 : M.Hayakawa
1911 ''' </remarks>'
1912 Function M% fnKEY_WAIT()
1913     fnKEY_WAIT = 0
1914     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
1915     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
1916     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
1917     '下記キー待ちの継続に反応させないため
1918     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
1919     Dly 0.2
1920     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
1921     MLocalLoopFlg=1
1922     While MLocalLoopFlg=1
1923         If M_In(11345) = 1 Then         '停止   M5345
1924             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
1925             fnKEY_WAIT = 1
1926             MLocalLoopFlg=-1
1927             Break
1928         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
1929             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
1930             fnKEY_WAIT = 2
1931             MLocalLoopFlg=-1
1932             Break
1933         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
1934             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
1935             fnKEY_WAIT = 3
1936             MLocalLoopFlg=-1
1937             Break
1938         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
1939             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
1940             fnKEY_WAIT = 4
1941             MLocalLoopFlg=-1
1942             Break
1943         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
1944             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
1945             fnKEY_WAIT = 5
1946             MLocalLoopFlg=-1
1947             Break
1948         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
1949             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
1950             fnKEY_WAIT = MRobotInit1%
1951             MLocalLoopFlg=-1
1952             Break
1953         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
1954             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
1955             fnKEY_WAIT = MRobotInit2%
1956             MLocalLoopFlg=-1
1957             Break
1958         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
1959             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
1960             fnKEY_WAIT = MRobotInit3%
1961             MLocalLoopFlg=-1
1962             Break
1963         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
1964             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
1965             fnKEY_WAIT = MRobotInit4%
1966             MLocalLoopFlg=-1
1967             Break
1968         Else
1969         EndIf
1970     WEnd
1971     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
1972     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
1973     Exit Function
1974 FEnd
1975 '
1976 '■ fnAUTO_CTL
1977 ''' <summary>
1978 ''' AUTOモードOFF、PLCからの開始待ち
1979 ''' </summary>
1980 ''' <remarks>
1981 ''' Date   : 2021/07/07 : M.Hayakawa
1982 ''' </remarks>
1983 Function M% fnAUTO_CTL
1984     fnAUTO_CTL = 0
1985     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1986     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
1987     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1988     '
1989     If M_Svo=0 Then             'サーボON確認
1990         Servo On
1991     EndIf
1992     Wait M_Svo=1
1993     Exit Function
1994 FEnd
1995 '
1996 '■ fnWindScreenOpen
1997 ''' <summary>
1998 ''' ウィンド画面の表示、非表示設定
1999 ''' </summary>
2000 '''<param name="%"></param>
2001 '''<param name="%"></param>
2002 '''<param name="%"></param>
2003 '''<param name="%"></param>
2004 ''' <remarks>
2005 ''' コメントD1001, D1002, D1003の設定
2006 ''' MWindReSet = 0     画面非表示
2007 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2008 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2009 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2010 ''' Date   : 2021/07/07 : M.Hayakawa
2011 ''' </remarks>
2012 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2013     If MCommentD1001 <> 0 Then              'コメント 0 は設定がないので確認
2014         M_Out16(12480) = MCommentD1001      'D1001 コメント
2015     EndIf
2016     '
2017     If MCommentD1002 <> 0 Then              'コメント 0 は設定がないので確認
2018         M_Out16(12496) = MCommentD1002      'D1002 コメント
2019     EndIf
2020     '
2021     If MCommentD1003 <> 0 Then              'コメント 0 は設定がないので確認
2022        M_Out16(12512) = MCommentD1003       'D1003 コメント
2023     EndIf
2024     '
2025     M_Out16(12448) = MScreenNo              '画面番号  M6448   10=エラー画面
2026     M_Out(12363) = 1 Dly 0.5                'ウィンド画面設定  M6362
2027     Exit Function
2028 FEnd
2029 '
2030 '■FnCtlValue2
2031 ''' <summary>
2032 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2033 ''' </summary>
2034 ''' <param name="MCtlNo%"></param>
2035 ''' <remarks>
2036 ''' Date : 2022/04/28 渡辺
2037 ''' </remarks>
2038 '''
2039 '''  1：投入数       ＋１
2040 '''  2：組立ＯＫ数   ＋１
2041 '''  3：供給機２吸着エラー数 ＋１　　組立NGから変更 2022/05/19 渡辺
2042 '''  4：供給機１吸着エラー数 ＋１
2043 ''' 99：読書開始信号 OFF
2044 '''
2045 Function M% FnCtlValue2(ByVal MCtlNo%)
2046     FnCtlValue2 = 1
2047     Select MCtlNo%
2048         Case 1        '投入数＋１
2049             M_Out(12569) = 0             '書込み開始信号OFF
2050             M_Out(12568) = 1             '読込み開始信号ON
2051             MInputQty = M_In16(11600)    '投入数受信
2052             MInputQty = MInputQty + 1    '投入数＋１
2053             M_Out16(12592) = MInputQty   '投入数送信
2054             M_Out(12569) = 1             '書込み開始信号ON
2055             Break
2056             '
2057         Case 2        '組立ＯＫ数＋１
2058             M_Out(12569) = 0             '書込み開始信号OFF
2059             M_Out(12568) = 1             '読込み開始信号ON
2060             MAssyOkQty = M_In16(11616)   '組立OK数受信
2061             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2062             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2063             M_Out(12569) = 1             '書込み開始信号ON
2064             Break
2065             '
2066         Case 3        '供給機２吸着エラー数＋１
2067             M_Out(12569) = 0                       '書込み開始信号OFF
2068             M_Out(12568) = 1                       '読込み開始信号ON
2069             MSuctionErrQty = M_In16(11632)         '供給機２吸着エラー数受信
2070             MSuctionErrQty = MSuctionErrQty + 1    '供給機２吸着エラー数＋１
2071             M_Out16(12624) = MSuctionErrQty        '供給機２吸着エラー数送信
2072             M_Out(12569) = 1                       '書込み開始信号ON
2073             Break
2074             '
2075         Case 4        '供給機１吸着エラー数＋１
2076             M_Out(12569) = 0                       '書込み開始信号OFF
2077             M_Out(12568) = 1                       '読込み開始信号ON
2078             MSuctionErrQty = M_In16(11648)         '供給機１吸着エラー数受信
2079             MSuctionErrQty = MSuctionErrQty + 1    '供給機１吸着エラー数＋１
2080             M_Out16(12640) = MSuctionErrQty        '供給機１吸着エラー数送信
2081             M_Out(12569) = 1                       '書込み開始信号ON
2082             Break
2083             '
2084         Case 99        '読書開始信号OFF
2085             M_Out(12568) = 0        '読込み開始信号OFF
2086             M_Out(12569) = 0        '書込み開始信号OFF
2087             Break
2088             '
2089     End Select
2090     Exit Function
2091 FEnd
2092 '
2093 '
2094 '■FnScreEroorCord
2095 ''' 電動ドライバーのエラーコードを含めたコメントを出す為のコメント番号の作成
2096 ''' 新規作成：2022/05/23 : 渡辺
2097 '''
2098 Function M% FnScreEroorCord()
2099     MScrewErrorCord% = 0
2100     If M_In(11252) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 1    '11252:E_driver Error Massage1 E1
2101     If M_In(11253) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 2    '11253:E_driver Error Massage1 E2
2102     If M_In(11254) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 4    '11254:E_driver Error Massage1 E3
2103     If M_In(11255) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 8    '11255:E_driver Error Massage1 E4
2104     If M_In(11258) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 16   '11258:E_driver Error Massage1 E5
2105     MScrewErrorCord% = MScrewErrorCord% * 10
2106     MScrewErrorCord% = MScrewErrorCord% + 500
2107     FnScreEroorCord = MScrewErrorCord%
2108     Exit Function
2109 FEnd
2110 '
2111 '
2112 'Insightによる画像処理検査実行（並列処理なし）
2113 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2114 '-------------------------------------------------------------------------------
2115 'Insightによる画像処理検査実行（並列処理なし）
2116 '   引数
2117 '       PInspPos()      ：検査位置
2118 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2119 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2120 '       MInspCnt%       ：検査位置数
2121 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2122 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2123 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2124 '   戻り値：整数
2125 '       0=異常終了、1=正常終了
2126 '
2127 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2128 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2129 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2130 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2131 '   20200410    :   検査グループ設定Retry追加
2132 '-------------------------------------------------------------------------------
2133     '----- 初期設定 -----
2134     Cnt 0                                                           '移動効率化解除(初期値=0)
2135     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2136 '    Cnt 1,0.1,0.1
2137     '変数宣言・初期化
2138     Def Inte MNum                                                   '検査番号(検査順1～)
2139     MNum% = 1                                                       '検査番号初期値設定
2140     Def Inte MEndFlg                                                '検査終了フラグ
2141     MEndFlg% = 0
2142     '
2143     '検査G番号設定要求・検査実行要求off
2144     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2145     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2146     'エラー番号クリア
2147     MInspErrNum = 0                                                 '検査実行エラー番号
2148     M_Out16(MOUT_InspErrNum) = MInspErrNum
2149     MInspNGStepNum = 0                                              '検査実行NGStep番号
2150     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2151     '
2152     'Insight Ready check?
2153     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2154         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2155         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2156         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2157         ISInspectionSingle = 0                                      '異常終了戻り値設定
2158         'Exit Function
2159     EndIf
2160     If MInspErrNum = 20 Then GoTo *ISInspectionSingle_End
2161     '
2162     '検査位置数確認
2163     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2164         MInspErrNum = 21                                            '検査データなし 21　引数<1
2165         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2166         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2167         ISInspectionSingle = 0                                      '異常終了戻り値設定
2168         'Exit Function
2169     EndIf
2170    If MInspErrNum = 21 Then GoTo *ISInspectionSingle_End
2171     '
2172     '
2173     '
2174     '----- メイン処理 -----
2175     '設定された検査位置数分の検査実行
2176     While( MEndFlg% = 0 )
2177         '----- 検査グループ番号設定Retry追加 20200410
2178         MSetGrNumRetryExitFlg = 0
2179         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2180         While( MSetGrNumRetryExitFlg = 0 )
2181         '----- 検査グループ番号設定Retry追加ここまで 20200410
2182             '
2183             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2184             '
2185             '----- 検査グループ番号設定 -----
2186             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2187             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2188             '
2189             '検査位置へ移動・移動完了待ち
2190             fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2191             Mov PInspPos( MNum% )                                       '移動
2192             fnAutoScreenComment(523)                                    '状態表示[画像処理検査中] 2022/05/09 渡辺
2193             Dly 0.2                                                     '移動完了後Delay 0.05>>0.2
2194             '
2195             '検査グループ番号設定終了確認
2196             M_Timer(1) = 0
2197             MExitFlg = 0
2198             While( MExitFlg = 0 )
2199                 '検査G設定正常終了?
2200                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2201                     MExitFlg = 1
2202                 '
2203                 '検査G設定異常終了?
2204                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2205                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2206                     If MInspErrNum = 0 Then                             '1回目のエラー?
2207                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2208                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2209                     EndIf
2210                     MExitFlg = 1
2211                 '
2212                 'timeoutチェック
2213                 ElseIf 1000 < M_Timer(1) Then
2214                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2215                     If MInspErrNum = 0 Then                             '1回目のエラー?
2216                         MInspErrNum = 12                                'timeout エラー番号=12
2217                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2218                     EndIf
2219                     MExitFlg = 1
2220                 EndIf
2221             WEnd
2222             '
2223             '検査G番号設定要求off
2224             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2225             '
2226             '----- 検査グループ設定Retry追加 20200410
2227             'NGなければ抜ける
2228             If MCurrentStepErr = 0 Then
2229                 MSetGrNumRetryExitFlg = 1
2230             Else
2231                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2232                 If MSetGrNumRetryCnt = 0 Then
2233                     MSetGrNumRetryExitFlg = 1
2234                 Else
2235                     'Retryへ　その前にDelay
2236                     Dly 0.5
2237                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2238                 EndIf
2239             EndIf
2240             '----- 検査グループ設定Retry追加ここまで 20200410
2241             '
2242         WEnd
2243         '
2244         '
2245         '
2246         '----- 検査実行 -----
2247         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2248             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2249                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2250                 MInspRetryExitFlg = 0
2251                 MRetryCnt = 2                                        'Retry回数設定
2252                 While( MInspRetryExitFlg = 0 )
2253                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2254                     '
2255                     '検査完了確認
2256                     MRetryCnt = MRetryCnt - 1
2257                     M_Timer(1) = 0
2258                     MExitFlg = 0
2259                     While( MExitFlg = 0 )
2260                     '検査完了待ち
2261                         '検査OK終了?
2262                         If M_In( MIN_IS_InspOK% ) = 1  Then
2263                             MJudgeOKFlg = 1                         '検査OKフラグON
2264                             MExitFlg = 1
2265                         '
2266                         '検査NG終了?
2267                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2268                             If MInspErrNum = 0 Then                 '1回目のエラー?
2269                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2270                                     MInspErrNum = 32                    '検査NG エラー番号=32
2271                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2272                                 EndIf
2273                             EndIf
2274                             MExitFlg = 1
2275                         '
2276                         '検査異常終了(IS timeout)?
2277                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2278                             If MInspErrNum = 0 Then                 '1回目のエラー?
2279                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2280                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2281                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2282                                 EndIf
2283                             EndIf
2284                             MExitFlg = 1
2285                         '
2286                         'timeoutチェック
2287                         ElseIf 3000 < M_Timer(1) Then
2288                             If MInspErrNum = 0 Then                 '1回目のエラー?
2289                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2290                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2291                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2292                                 EndIf
2293                             EndIf
2294                             MExitFlg = 1
2295                         EndIf
2296                     WEnd
2297                     '
2298                     '検査開始要求off
2299                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2300                     '
2301                     'OKなら抜ける
2302                     If MJudgeOKFlg = 1 Then
2303                         MInspRetryExitFlg = 1
2304                     Else
2305                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2306                         If MRetryCnt = 0 Then
2307                             MInspRetryExitFlg = 1
2308                         Else
2309                             'Retryへ　その前にDelay
2310                             Dly 0.3
2311                         EndIf
2312                     EndIf
2313                     '
2314                 WEnd
2315             EndIf
2316         EndIf
2317         '
2318         '
2319         '
2320         MNum% = MNum% + 1                                           '検査Step+1
2321         '検査終了確認　検査終了フラグセット
2322         If (MInspCnt% < MNum% ) Then
2323             MEndFlg% = 1                                            '検査終了フラグセット
2324         EndIf
2325         'NG発生時続行時処理
2326         If MInspErrNum <> 0 Then                                    'NGあり?
2327             If MNgContinue% <> 1 Then                               'NG続行?
2328                 MEndFlg% = 1                                        '検査終了フラグセット
2329             EndIf
2330         EndIf
2331     WEnd
2332     '
2333     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2334     If 0 < MZAxis% Then
2335         PCurrentPos = P_Curr                                        '現在位置取得
2336         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2337         fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2338         Mvs PCurrentPos                                             '現在位置上空へ移動
2339     EndIf
2340     '
2341     '戻り値設定
2342     If MInspErrNum = 0 Then
2343         ISInspectionSingle = 1                                      '正常終了戻り値設定
2344     Else
2345         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2346         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2347         ISInspectionSingle = 0                                      '異常終了戻り値設定
2348     EndIf
2349 '
2350 *ISInspectionSingle_End
2351 Fine 0 , P
2352     Exit Function
2353 FEnd
2354 '
2355 '■fnAutoScreenComment
2356 ''' <summary>
2357 ''' メイン画面の動作状況表示
2358 ''' コメントD1005の設定
2359 ''' </summary>
2360 '''<param name="McommentD1005%">コメントID</param>
2361 ''' <remarks>
2362 ''' Date   : 2021/07/07 : M.Hayakawa
2363 ''' </remarks>
2364 Function fnAutoScreenComment(ByVal McommentD1005%)
2365     M_Out16(12576) = McommentD1005%
2366     Exit Function
2367 FEnd
2368 '
2369 '■fnRoboPosChk
2370 ''' <summary>
2371 ''' 最後に終了したロボットポジションの確認
2372 ''' </summary>
2373 '''<param name="MINNumber%">入力番号</param>
2374 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2375 '''<param name="MTimeCnt&">タイムアウト時間</param>
2376 ''' PLCに保続した番号を読込み、確認
2377 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2378 '''<returns>整数 0:タイムアウト 1:OK</returns>
2379 ''' <remarks>
2380 ''' Date   : 2021/07/07 : M.Hayakawa
2381 ''' </remarks>
2382 Function M% fnRoboPosChk
2383     fnRoboPosChk = 0
2384     MRet = fnStepRead()
2385     '初期位置でないと判断した場合
2386     'ウィンド画面切換え
2387     If MRBTOpeGroupNo > 5 Then
2388         '下記キー待ちの継続に反応させないため
2389         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2390         Dly 0.2
2391         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2392         Dly 1.5
2393         '
2394         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2395         '
2396         MLoopFlg% = 1
2397         While MLoopFlg% = 1
2398             '
2399             '
2400             MKeyNumber% = fnKEY_WAIT()
2401             Select MKeyNumber%
2402                 Case Is = MAbout%       '停止
2403                     M_20# = MAbout%
2404                     MLoopFlg% = -1
2405                     Break
2406                 Case Is = MNext%        '次へ
2407                     'MLoopFlg% = -1
2408                     Break
2409                 Case Is = MContinue%    '継続
2410                     M_20# = MContinue%
2411                     MLoopFlg% = -1
2412                     Break
2413                 Default
2414                     Break
2415             End Select
2416         WEnd
2417     EndIf
2418     '
2419     If M_20# = MContinue% Then                              '継続ボタンが押された場合
2420         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
2421         Ovrd 5                                   '低速オーバーライド値設定
2422         Select MRBTOpeGroupNo
2423             Case Is = 5                          '何もしない
2424                 Break
2425             Case Is = 10                         '初期位置へ戻す
2426                 'Mov PTEST001
2427                 Break
2428             Case Is = 15                         '初期位置へ戻す
2429                 'Mov PTEST002
2430                 Dly 0.5
2431                 'Mov PTEST001
2432                 Dly 0.5
2433                 Break
2434             Default
2435                 Break
2436         End Select
2437         '
2438         Ovrd M_NOvrd                            'システムの初期値を設定
2439         M_Out(12364) = 1                        'toPLC_データ保存ON
2440         MRBTOpeGroupNo = 5
2441         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
2442         Dly 1.0
2443         M_Out(12364) = 0                        'toPLC_データ保存OFF
2444         fnRoboPosChk = 1                        '初期位置動作実行
2445         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
2446     EndIf
2447     Exit Function
2448 FEnd
2449 '
2450 '■frInCheck
2451 ''' <summary>
2452 ''' センサーINチェック
2453 ''' </summary>
2454 '''<param name="MINNumber%">入力番号</param>
2455 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2456 '''<param name="MTimeCnt&">タイムアウト時間</param>
2457 '''<returns>整数 0:タイムアウト 1:OK</returns>
2458 ''' <remarks>
2459 ''' Date   : 2021/07/07 : M.Hayakawa
2460 ''' </remarks>
2461 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2462     M_Timer(4) = 0
2463     MloopFlg = 0
2464     While MloopFlg = 0
2465         MCrtTime& = M_Timer(4)
2466         If M_In(MINNumber%) = MCMPFLG% Then
2467             MloopFlg = 1
2468             frInCheck = 1
2469         ElseIf MCrtTime& > MTimeCnt& Then
2470             MloopFlg = 1
2471             frInCheck = 0
2472         EndIf
2473     WEnd
2474     Exit Function
2475 FEnd
2476 '-----------------------------------------------
2477 '
2478 'ねじ締め機通信確認
2479 '
2480 '-----------------------------------------------
2481 Function M% fScewTcomChk
2482     fScewTcomChk = 0
2483     '通信確認送信
2484     M_Out(MOUT_ScwT_ComChk%) = MOn%
2485     '通信確認受信待機
2486     Wait M_In(MIN_ScwT_comOK%) = MOn%
2487     '通信確認送信終了
2488     M_Out(MOUT_ScwT_ComChk%) = MOff%
2489     Exit Function
2490 FEnd
2491 '
2492 '
2493 '-----------------------------------------------
2494 '
2495 'ねじ締め開始送信
2496 '
2497 '-----------------------------------------------
2498 Function M% fScewTStart
2499     fScewTStart = 0
2500     'ねじ締め開始待機を受信
2501     Wait M_In(MIN_ScwT_STRec%) = MOn%
2502     Dly 0.1
2503     'ねじ締め開始受信を送信
2504     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msecパルス
2505     Exit Function
2506 FEnd
2507 '
2508 '
2509 '-----------------------------------------------
2510 '
2511 'ねじ締め完了受信
2512 '
2513 '-----------------------------------------------
2514 Function M% fScewTFinish
2515     fScewTFinish = 0
2516     'ねじ締め完了待機を受信
2517     Wait M_In(MIN_ScwT_Fin%) = MOn%
2518     Dly 0.1
2519     'ねじ締め完了受信を送信
2520     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msecパルス
2521     Exit Function
2522 FEnd
2523 '
2524 '
2525 '-----------------------------------------------
2526 '
2527 '条件xx停止受信
2528 '
2529 '-----------------------------------------------
2530 Function M% fScewTCaseStop(ByVal MCase%())
2531     fScewTCaseStop = 0
2532     '条件xx停止を受信
2533     Wait M_In(MCase%(1)) = MOn%
2534     Dly 0.1
2535     '条件xx停止受信を送信
2536     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msecパルス
2537     Exit Function
2538 FEnd
2539 '
2540 '-----------------------------------------------
2541 '
2542 '再開始受信
2543 '
2544 '-----------------------------------------------
2545 Function M% fScewTReStart()
2546     fScewTReStart = 0
2547     '再開始を受信
2548     Wait M_In(MIN_ScwT_ReST%) = MOn%
2549     Dly 0.1
2550     '再開始受信を送信
2551     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msecパルス
2552     Exit Function
2553 FEnd
2554 '
2555 '■fErrorProcess
2556 '<summary>
2557 'エラー処理
2558 '</summary>
2559 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
2560 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
2561 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
2562 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
2563 '<make>
2564 '2021/11/5 中村天哉
2565 '</make>
2566 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2567     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
2568     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
2569     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
2570     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
2571 *RETRY_ERR_PROCESS
2572      M_20# = MClear%     '初期化
2573 '        'エラー処理記述
2574         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2575 '        'GOT KEY入力待ち
2576         MKeyNumber = fnKEY_WAIT()
2577 '        '
2578         If MKeyNumber = MAbout% Then   '停止を選択した場合
2579             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2580  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2581             Break
2582          '
2583         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
2584             M_20# = MContinue%            'M_20# プログラム間共通外部変数
2585  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2586         '
2587         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
2588             M_20# = MNext%            'M_20# プログラム間共通外部変数
2589  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2590          '
2591         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
2592             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
2593  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2594             Break
2595         '
2596         EndIf
2597         '
2598         '
2599         '
2600         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2601         fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2602     Exit Function
2603 FEnd
2604 '
2605 '■InitialZone
2606 ''' <summary>
2607 ''' 現在位置から上空に待避し、初期位置に戻る
2608 ''' </summary>
2609 ''' <remarks>
2610 ''' Date : 2021/12/2 : M.Hayakawa
2611 ''' Update:2022/06/2 : M.Hayakawa 他工程の非常停止復帰に合わせて変更
2612 ''' </remarks>
2613 Function fnInitialZone()
2614     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中]
2615 '
2616     Ovrd 5
2617 ' 上空退避
2618     PActive = P_Curr
2619     Pmove = PActive
2620 '
2621     If PActive.X > 580 Then
2622         Pmove.Z =380        'パレット上に腕を伸ばしているときは500まで上げられない為、例外処置
2623     Else
2624         Pmove.Z =500        '上記以外はZ:500まで持ち上げ
2625     EndIf
2626 '
2627     Mvs Pmove
2628     Mov PInitialPosition
2629 ' ロックを開放
2630     InitialState()
2631 ' 一旦停止
2632     fErrorProcess(20,70,256,0)
2633     Exit Function
2634 FEnd
2635 '
2636 '■InitialState
2637 ''' <summary>
2638 ''' ハンド、治具を初期位置にする
2639 ''' </summary>
2640 ''' <returns>   0 : OK
2641 '''             1 : NG
2642 ''' </returns>
2643 ''' <remarks>
2644 ''' Date : 2021/12/2 : M.Hayakawa
2645 ''' </remarks>
2646 Function M% InitialState()
2647     InitialState = 0
2648     '製品位置決め解除
2649     M_Out(12261)=1 Dly 0.3      'FANクランプ出端パルス出力
2650     'Wait M_In(11271)=1          'FANクランプ出端検出(修正につきコメントアウト(8/26中村))
2651     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'FANクランプ出端検出(8/26中村)
2652     If MRtn = 0 Then
2653         fErrorProcess(11,234,284,0)
2654         Select M_20#
2655             Case MAbout%            '停止か
2656                 InitialState = 1
2657                 Break
2658             Case MNgProcess%        'NGが押された場合
2659                 InitialState = 0
2660                 Break
2661             Case MContinue%
2662                 M_20# = MClear%
2663                 InitialState = 0
2664                 Break
2665             Case MNext%
2666                 M_20# = MClear%
2667                 InitialState = 0
2668                 Break
2669         End Select
2670     EndIf
2671     '
2672     M_Out(12259)=1 Dly 0.3      'プッシュCY用SV戻端パルス出力
2673     'Wait M_In(11269)=1          'プッシュ戻端検出(修正につきコメントアウト(8/26中村))
2674     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)    'プッシュ戻端検出
2675     If MRtn = 0 Then
2676         fErrorProcess(11,234,284,0)
2677         Select M_20#
2678             Case MAbout%            '停止か
2679                 InitialState = 1
2680                 Break
2681             Case MNgProcess%        'NGが押された場合
2682                 InitialState = 1
2683                 Break
2684             Case MContinue%
2685                 M_20# = MClear%
2686                 InitialState = 0
2687                 Break
2688             Case MNext%
2689                 M_20# = MClear%
2690                 InitialState = 0
2691                 Break
2692         End Select
2693     EndIf
2694     '
2695     M_Out(12257)=1 Dly 0.3      '位置決めCY用SV戻端パルス出力
2696     'Wait M_In(11267)=1          '位置決め戻端検出(修正につきコメントアウト(8/26中村))
2697     MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '位置決め戻端検出(8/26中村)
2698     If MRtn = 0 Then
2699         fErrorProcess(11,234,284,0)
2700         Select M_20#
2701             Case MAbout%            '停止か
2702                 InitialState = 1
2703                 Break
2704             Case MNgProcess%        'NGが押された場合
2705                 InitialState = 1
2706                 Break
2707             Case MContinue%
2708                 M_20# = MClear%
2709                 InitialState = 0
2710                 Break
2711             Case MNext%
2712                 M_20# = MClear%
2713                 InitialState = 0
2714                 Break
2715         End Select
2716     EndIf
2717     Exit Function
2718 FEnd
2719 '
2720 '■fnTorqueCheck
2721 ''' <summary>
2722 ''' トルクチェック動作用のメイン
2723 ''' </summary>
2724 ''' <remarks>
2725 ''' Date   : 2021/12/21 : H.AJI
2726 ''' </remarks>'
2727 Function M% fnTorqueCheck
2728     'トルクチェック中送信  搬送系停止
2729     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
2730     '
2731     fnTorqueCheck = 0
2732     Ovrd 20
2733     Mov PInitialPosition              '初期位置移動
2734     Ovrd 100
2735     '下記キー待ちの継続に反応させないため
2736     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2737     Dly 0.2
2738     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2739     '
2740     'M6340  トルクチェック受信
2741     M_Out(12340) = 1 Dly 1.0                'トルクチェック受信 M6340
2742     Dly 1.0
2743     M_Out(12340) = 0
2744     '
2745     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
2746     '
2747     MLoopFlg = 1
2748     While MLoopFlg = 1
2749         '
2750         Mov PInitialPosition              '初期位置移動
2751         '
2752         MKeyNumber = fnKEY_WAIT()
2753         Select MKeyNumber
2754             Case Is = 1           '停止
2755                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
2756                 Dly 1.0
2757                 M_Out(12343) = 0
2758                 Ovrd 20
2759                 Mov PTicketRead_1
2760                 Ovrd 100
2761                 M_20# = 1
2762                 MLoopFlg = -1
2763                 Break
2764             Case Is = 2           '次へ
2765                 Break
2766             Case Is = 3           '継続
2767                 Break
2768             Case Is = 4           'トルクチェック開始
2769                 M_Out(12545) = 1    ' toPLC_PCトルクチェック1要求受信(M315)
2770                 M_Out(12342) = 1 Dly 1.0    'トルクチェック開始要求受信 M6342
2771                 fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
2772                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
2773                 MRet = fnMoveTorquePosi()
2774                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
2775                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2776                 Break
2777             Default
2778                 Break
2779         End Select
2780     WEnd
2781     '
2782     'トルクチェック中停止送信
2783     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
2784     '
2785     'ロボットの位置を元に戻す
2786     '
2787     Exit Function
2788  FEnd
2789  '
2790 '
2791 '
2792 '---------------------------
2793 '
2794 '    メイン画面の表示、非表示設定
2795 '         コメントD1001, D1002, D1003の設定
2796 '           MWindReSet = 0     画面非表示
2797 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
2798 '           MWindErrScr = 10    エラー画面 D1001, D1002
2799 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2800 '
2801 '---------------------------
2802 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2803     fnMainScreenOpen = 0
2804     '
2805    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2806         M_Out16(12480) = MCommentD1001            'D1001 コメント
2807     EndIf
2808     '
2809     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2810         M_Out16(12496) = MCommentD1002            'D1002 コメント
2811     EndIf
2812     '
2813     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2814         M_Out16(12512) = MCommentD1003            'D1003 コメント
2815     EndIf
2816     '
2817     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2818     M_Out(12362) = 1                         'ウィンド画面設定  M6362
2819     Dly 0.5
2820     M_Out(12362) = 0                         'ウィンド画面設定
2821     Exit Function
2822 FEnd
2823 '
2824 '■Main
2825 ''' <summary>
2826 ''' トルクチェック実動作
2827 ''' </summary>
2828 ''' <remarks>
2829 ''' Date   : 2021/12/21 : H.AJI
2830 ''' </remarks>'
2831 Function M% fnMoveTorquePosi
2832      fnMoveTorquePosi = 0
2833      Ovrd 50
2834      Mov PTorqueCheck_1 'トルクチェックメーター上空へ移動
2835     '
2836     Spd M_NSpd
2837 '-------------      ドライバーRST
2838     M_Out(12240)=0     'ドライバーOFF CCW
2839     M_Out(12241)=0     'ドライバーOFF CW
2840     M_Out(12242)=1     'ドライバー解除 C1
2841     M_Out(12243)=1     'ドライバー解除 C2
2842     M_Out(12245)=0     'プログラム解除 F1/プログラム2
2843 '---------------------------------------
2844 '[P-11]
2845 '--------------------------------------------------------------   【トルクチェック 0.4N - P11】
2846     Mov PTorqueCheck, -50                     ' トルク-1　置き位置上空 50mm へ移動
2847     Dly 0.1
2848 '-----------------------
2849    'Cnt 0                           'Cnt動作-2　終了
2850 '-----------------------
2851     Mov PTorqueCheck , -5                      'トルク-1　置き位置上空 5mm へ移動
2852     Dly 0.2
2853 '-----------------------
2854     ProgramBankSet(1,3)
2855     M_Out(12241)=0                   'ドライバーOFF  CW
2856     'Dly 0.1
2857 '--------------------------------
2858     Ovrd 40
2859    'Dly 0.1
2860 '--------------------------------  ネジ締め速度設定
2861     Spd 14                            'ライド 100-40 100% :Spd 12
2862     Dly 0.1
2863 '--------------------------------
2864 '--------------------------------
2865 '---------------------------------【ねじ締め動作】
2866 '
2867     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '移動中エラー検出
2868    Mvs PTorqueCheck               'トルクチェック位置へ移動
2869     Dly 0.3                          '動作安定待ち
2870    M_Out(12241)=1                   'ドライバーON  CW
2871 '
2872     Wait M_In(11584)=1                '完了/エラー検出
2873     Dly 0.1
2874     Spd M_NSpd
2875    'Ovrd 20
2876     If M_In(11256)=1 Then *LBL1       'ネジトータルエラー検出
2877     Wait M_In(11257)=1                'ネジ完了SC
2878 '---------------------------------
2879     Dly 0.1
2880     M_Out(12241)=0                    'ドライバーOFF CW
2881     Dly 0.1
2882     M_Out(12242)=0                    'ドライバー解除 C1
2883     Dly 0.1
2884     M_Out(12243)=0                    'ドライバー解除 C2 (バンク3)
2885     Dly 0.1
2886     M_Out(12245)=0                    'プログラム2解除 F1
2887 '--------------------------------------------------------------   【トルクチェック 0.4N - P11ここまで】
2888 '
2889     Mvs PTorqueCheck,-60                       'あえてmov から変更
2890     Dly 0.1
2891 '--------------------------------------------------------------
2892    'Ovrd 80
2893 '--------------------------------------------------------------
2894 '---------------------------------------
2895 '---------------------------------------
2896 '---------------------------------------エラー離脱処理
2897    *LBL1
2898    Fsc Off            '力覚センサ　Off   *STEP1は不要
2899    Mvs ,-100
2900    M_Out(12241)=0     'ドライバーOFF CW
2901    Dly 0.1
2902    M_Out(12242)=0     'ドライバー解除 C1
2903    Dly 0.1
2904    M_Out(12243)=0     'ドライバー解除 C2 (バンク3)
2905    Dly 0.1
2906    M_Out(12245)=0     'プログラム解除 F1
2907 '---------------------------------------
2908 '---------------------------------------
2909 '-------------
2910    'Mov PInitPos19049
2911    Dly 0.1
2912 '
2913 '
2914     Exit Function
2915 FEnd
2916 '
2917 '■Main
2918 ''' <summary>
2919 ''' 組立動作用のメイン
2920 ''' </summary>
2921 ''' <remarks>
2922 ''' Date   : 2021/07/07 : M.Hayakawa
2923 ''' </remarks>'
2924 Function Main
2925     MopeNo = M_21#         '外部変数にて動作番号代入
2926     '
2927     If M_Svo=0 Then
2928         Servo On
2929     EndIf
2930     Wait M_Svo=1
2931 '組立スタート日付時刻要求パルスON
2932     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2933 'パトライト操作
2934     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
2935     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
2936     '
2937     M_20# = 0                                   'KEY入力初期化
2938     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
2939     MRet% = 0
2940 '復帰動作　実行・未実行判別      2022/03/22 渡辺 作成
2941     PActive = P_Curr                    '現在位置を取得
2942     MRecoveryPass% = 0
2943     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2944         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2945             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2946             MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
2947         EndIf
2948     EndIf
2949     EndIf
2950     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2951         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2952             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2953                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
2954             EndIf
2955         EndIf
2956     EndIf
2957     If MRecoveryPass% = 0 Then
2958         fnInitialZone()        '復帰動作パスフラグが立っていない時は復帰動作を実行
2959     EndIf
2960 '
2961     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
2962         M_Out(12364) = 1            'toPLC_データ保存ON
2963 'トルクチェック
2964         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2965             MRet% = fnTorqueCheck()
2966             Break
2967         Else
2968 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
2969 '                MRtn = InspInit()               '画像処理初期化処理
2970 '            EndIf
2971             '
2972            M_20# = MClear%                    '初期化
2973 '組立開始
2974             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2975 '                MRet% = fnAssyStart()
2976                 fnAssyStart()
2977             Else
2978                 M_20# = MPass%
2979             EndIf
2980 '組立終了日付時刻
2981             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
2982             Wait M_In(11572) = 1            '日付取得完了
2983             Dly 0.1
2984             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
2985             '  KEY入力が何もない場合 OKと判断
2986             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
2987 ' 後工程へフラグ出力
2988             If M_20# <> MAbout% Then
2989                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
2990             ElseIf M_20# = MPass% Then
2991                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
2992             EndIf
2993 'About(停止)以外はOKを出力（パレット降下）
2994 '            If M_20# <> MAbout% Then
2995 '                M_Out(12339) = 1 Dly 0.5    'M6339  toPLC_RBT完了パルス出力
2996 '            EndIf
2997 '            M_Out(12346) = 0                ' M6346 toPLC_組立開始受信 OFF
2998 'PIASに組立完了書込み
2999             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3000                 If M_20# = MPass% Then
3001                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3002                 Else
3003                     'KEY入力がNGの場合
3004                     If M_20# = MNgProcess% Then
3005 '                        M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3006                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3007                         MRet% = fnPiasWrite(MNG%)
3008                        nAssyNgQty = nAssyNgQty + 1
3009                     EndIf
3010                     '
3011                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3012                     If M_20# = MAssyOK% Then
3013                             '-----------------------
3014                             'D732 -> D2600 コピー要求
3015                             M_Out(12566) = 1
3016 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3017                             M_Out(12566) = 0
3018                             '
3019                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3020                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3021                             '基板番号照合(PPは未使用）
3022 '                            MRet% = fnPCBNumberCheck()
3023                         Else
3024                             MRet% = 1
3025                         EndIf
3026                         '
3027                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3028                             If M_20# <> MAbout% Then
3029                                 '工程履歴OK書き込み
3030                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3031                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3032                                 MRet% = fnPiasWrite(MOK%)
3033                                 nAssyOkQty = 0
3034                                 nAssyOkQty = nAssyOkQty + 1
3035                             Else
3036                                 nAssyOkQty = nAssyOkQty + 1
3037                             EndIf
3038                         EndIf
3039                     EndIf
3040 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3041 '                    MRet% = fnPiasWrite(MOK%)
3042                 EndIf
3043             Else
3044                 nAssyOkQty = nAssyOkQty + 1
3045             EndIf
3046             '
3047             '組立終了日付時刻解除
3048             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3049             '投入数、組立OK数、組立NG数書込み
3050 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3051             '
3052 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3053 '                '画像処理終了処理
3054 '                MRtn = InspQuit()
3055 '            EndIf
3056         EndIf
3057         M_Out(12364) = 0                          'toPLC_データ保存OFF
3058     EndIf
3059 'パトライト操作
3060     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3061     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3062 'GOT表示
3063     fnAutoScreenComment(93)  'AUTO画面 工程完了
3064 '    M_Out(12339) = 1 Dly 0.5        ' M6339 toPLC_RBT完了パルスON
3065 '    M_Out(12346) = 0        'M6346  toPLC_AssY開始受信 OFF
3066 '
3067 FEnd
3068 End
3069 '
3070 '
3071 'おまじないコメント
3072 '絶対削除するな
3073 '
3074 '
3075 '
3076 '
3077 '
PActive=(602.000,-150.750,450.000,-180.000,-0.020,90.000)(7,0)
PCalcGetFanScrew=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PCalcGetMainScrew=(0.000,0.000,-1.200,0.000,0.000,0.000,0.000,0.000)
PGyroPcbRead=(315.060,-65.410,414.000,160.000,0.000,90.000)(7,0)
PGyroPcbRead_1=(329.620,-57.710,450.000,-180.000,0.000,90.000)(7,0)
PInitialPosition=(300.000,0.000,450.000,-180.000,0.000,-180.000)(7,0)
PMainPcbRead=(309.580,-174.170,413.000,-180.000,0.000,180.000)(7,0)
PMainPcbRead_1=(309.580,-174.170,450.000,-180.000,0.000,180.000)(7,0)
Pmove=(602.000,-150.750,380.000,180.000,-0.020,90.000)(7,0)
PParts1Check=(313.200,-30.000,435.000,180.000,0.000,-180.000)(7,0)
PParts1Check_1=(313.200,-30.000,480.000,-180.000,0.000,180.000)(7,0)
PParts2Check=(348.350,127.390,413.000,180.000,0.000,-180.000)(7,0)
PParts2Check_1=(348.350,127.390,450.000,180.000,0.000,180.000)(7,0)
PParts3Check=(538.880,54.120,420.000,180.000,0.000,-180.000)(7,0)
PParts3Check_1=(538.880,54.120,460.000,-180.000,0.000,180.000)(7,0)
PParts4Check=(520.000,-46.000,397.000,-180.000,0.000,180.000)(7,0)
PParts4Check_1=(520.000,-46.000,460.000,-180.000,0.000,-180.000)(7,0)
PScrewFan1=(316.840,123.450,303.780,-180.000,0.000,90.000)(7,0)
PScrewFan1_0=(316.840,123.450,313.780,-180.000,0.000,90.000)(7,0)
PScrewFan1_1=(316.840,123.450,370.000,-180.000,0.000,90.000)(7,0)
PScrewFan2=(348.670,91.640,303.780,-180.000,0.000,90.000)(7,0)
PScrewFan2_0=(348.670,91.640,313.780,-180.000,0.000,90.000)(7,0)
PScrewFan2_1=(348.670,91.640,370.000,-180.000,0.000,90.000)(7,0)
PScrewMain1=(304.960,-26.420,303.500,-180.000,0.000,90.000)(7,0)
PScrewMain1_0=(304.960,-26.420,309.500,-180.000,0.000,90.000)(7,0)
PScrewMain1_1=(304.960,-26.420,380.000,-180.000,0.000,90.000)(7,0)
PScrewMain2=(304.930,-175.170,304.630,-180.000,0.000,90.000)(7,0)
PScrewMain2_0=(304.930,-175.170,310.630,180.000,0.000,90.000)(7,0)
PScrewMain2_1=(304.930,-175.170,380.000,-180.000,0.000,90.000)(7,0)
PScrewMain3=(367.020,-180.760,303.900,-180.000,0.000,90.000)(7,0)
PScrewMain3_0=(367.020,-180.760,309.900,-180.000,0.000,90.000)(7,0)
PScrewMain3_1=(367.020,-180.760,380.000,-180.000,0.000,90.000)(7,0)
PScrewMain4=(379.080,23.990,303.900,-180.000,0.000,60.000)(7,0)
PScrewMain4_0=(379.080,23.990,309.900,-180.000,0.000,60.000)(7,0)
PScrewMain4_1=(379.080,23.990,380.000,-180.000,0.000,60.000)(7,0)
PScrewMain5=(384.980,-105.100,322.220,-180.000,0.000,90.000)(7,0)
PScrewMain5_0=(384.980,-105.100,333.620,-180.000,0.000,90.000)(7,0)
PScrewMain5_1=(384.980,-105.100,380.000,-180.000,0.000,90.000)(7,0)
PScrewMain6=(325.350,-79.150,323.270,180.000,0.000,90.000)(7,0)
PScrewMain6_0=(325.350,-79.150,333.670,-180.000,0.000,90.000)(7,0)
PScrewMain6_1=(325.350,-79.150,380.000,-180.000,0.000,90.000)(7,0)
PScrewSupplyFan=(233.050,389.510,338.720,-180.000,0.000,179.990)(7,0)
PScrewSupplyFan_1=(233.050,389.510,380.000,-180.000,0.000,-180.000)(7,0)
PScrewSupplyFan_2=(166.050,146.930,400.000,-180.000,0.000,128.050)(7,0)
PScrewSupplyFan_9=(127.520,411.680,432.420,-180.000,0.000,-180.000)(7,0)
PScrewSupplyMain=(102.770,194.030,338.050,180.000,0.000,-180.000)(7,0)
PScrewSupplyMain_1=(102.770,194.030,380.000,-180.000,0.000,180.000)(7,0)
PScrewSupplyMain_2=(166.050,146.930,447.340,-180.000,0.000,128.050)(7,0)
PScrewSupplyMain_9=(-3.190,216.640,432.440,180.000,0.000,-180.000)(7,0)
PTemp=(602.000,-150.750,450.000,-180.000,-0.020,90.000,0.000,0.000)(7,0)
PTicketRead=(602.000,-150.750,378.000,180.000,-0.020,90.000)(7,0)
PTicketRead_1=(602.000,-150.750,450.000,180.000,-0.020,90.000)(7,0)
PTorqueCheck=(143.660,-242.000,340.000,-180.000,-0.010,90.000)(7,0)
PTorqueCheck_1=(143.660,-242.000,360.000,-180.000,-0.010,90.000)(7,0)
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
PGetScrewPosTemp(1)=(102.770,194.030,380.000,-180.000,0.000,180.000,0.000,0.000)(7,0)
PGetScrewPosTemp(2)=(166.050,146.930,447.340,-180.000,0.000,128.050,0.000,0.000)(7,0)
PGetScrewPosTemp(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPosTemp(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPosTemp(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPosTemp(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPosTemp(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPosTemp(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPosTemp(9)=(-3.190,216.640,432.440,180.000,0.000,-180.000,0.000,0.000)(7,0)
PGetScrewPosTemp(10)=(102.770,194.030,338.050,180.000,0.000,-180.000,0.000,0.000)(7,0)
PInspPosition(1)=(313.200,-30.000,435.000,180.000,0.000,-180.000,0.000,0.000)(7,0)
PInspPosition(2)=(348.350,127.390,413.000,180.000,0.000,-180.000,0.000,0.000)(7,0)
PInspPosition(3)=(538.880,54.120,420.000,180.000,0.000,-180.000,0.000,0.000)(7,0)
PInspPosition(4)=(520.000,-46.000,397.000,-180.000,0.000,180.000,0.000,0.000)(7,0)
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
PScrewPosTemp(1)=(384.980,-105.100,380.000,-180.000,0.000,90.000,0.000,0.000)(7,0)
PScrewPosTemp(2)=(384.980,-105.100,333.620,-180.000,0.000,90.000,0.000,0.000)(7,0)
PScrewPosTemp(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPosTemp(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPosTemp(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPosTemp(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPosTemp(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPosTemp(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPosTemp(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPosTemp(10)=(384.980,-105.100,322.220,-180.000,0.000,90.000,0.000,0.000)(7,0)
