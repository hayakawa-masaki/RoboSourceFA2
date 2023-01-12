1 ' ===================================
2 '
3 '  21054001 STEP5 Assy6プログラム
4 '
5 ' 作成者：自動化T
6 ' 作成日：2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1から流用
8 '
9 ' Ver 0.3 2021.12.17 画像検査関数ISInspection→ISInspectionSingle、画像検査追加 file:210542003
10 ' Ver 0.4 2022.03.06 工程5との進入禁止許可強化
11 ' Ver 0.5 2022.04.08 BRACKET供給機　搬送待ち MSETTIMEOUT05&(5秒)→ MSETTIMEOUT08&(8秒)に変更
12 ' ===================================
13 '===== <Insight定数> =====
14 '===== <Insight変数定義> =====
15 Dim PInspPosition(30)               '画像処理Function引渡し用位置変数
16 Dim MInspGroup%(30)                 '画像処理Function引渡し用変数
17 Def Inte MIN_IS_Ready               '【入力IO】Insight準備OK
18 Def Inte MIN_IS_JobLoadOK           '【入力IO】Insightジョブロード正常終了
19 Def Inte MIN_IS_JobLoadNG           '【入力IO】Insightジョブロード異常終了
20 Def Inte MIN_IS_InspGSetOK          '【入力IO】Insight検査グループ番号設定正常終了
21 Def Inte MIN_IS_InspGSetNG          '【入力IO】Insight検査グループ番号設定異常終了
22 Def Inte MIN_IS_InspOK              '【入力IO】Insight検査OK
23 Def Inte MIN_IS_InspNG              '【入力IO】Insight検査NG
24 Def Inte MIN_IS_InspErr             '【入力IO】Insight検査異常終了
25 Def Inte MIN_IS_InspCapDone         '【入力IO】Insight検査画像取込完了
26 '
27 Def Inte MIN_IS_ErrNum              '【入力IO】Insight処理エラー番号取得開始アドレス(16bit)
28 'Output Signal
29 Def Inte MOUT_IS_JobLoadReq         '【出力IO】Insight JOBロード要求
30 Def Inte MOUT_IS_InspGSetReq        '【出力IO】Insight 検査グループ番号設定要求
31 Def Inte MOUT_IS_Insp               '【出力IO】Insight 検査実行要求
32 '
33 Def Inte MOUT_IS_JobNum             '【出力IO】Insight JOB番号設定開始アドレス(16bit)
34 Def Inte MOUT_IS_InspGNum           '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
35 '
36 Def Inte MOUT_InspErrNum            '【出力IO】検査実行エラー番号開始アドレス(16bit)
37 Def Inte MOUT_InspNGStepNum         '【出力IO】検査実行NGStep番号開始アドレス(16bit)
38 Def Inte MOUT_OKNG                 '
39 '作業用変数
40 Def Inte MInspErrNum                '検査実行エラー番号
41 Def Inte MInspNGStepNum             '検査実行NGStep番号
42 Def Inte MRtn                       'Function戻り値取得用
43 Def Inte MRtn2                      'Function戻り値取得用
44 Def Inte MRet3                      'Function戻り値取得用
45 Def Inte MGRtn                      'Function戻り値取得用 ネジ供給機
46 Def Inte MInspErrNumSub             '検査実行エラー番号sub　20190820追加
47 Def Inte MovrdA                     'ネジ締めOvrd 可変用
48 Def Float MSpdA                     'ネジ締めSpd　可変用
49 Def Pos PTemp                       'ネジ締め上空位置計算用
50 Def Inte MKeyNum                    'KEY入力受け取り用(追加12/20中村)
51 '===== <Insight変数設定> =====
52 MIN_IS_Ready%        =   11380      '【入力IO】Insight準備OK
53 MIN_IS_JobLoadOK%    =   11381      '【入力IO】Insightジョブロード正常終了
54 MIN_IS_JobLoadNG%    =   11382      '【入力IO】Insightジョブロード異常終了
55 MIN_IS_InspGSetOK%   =   11383      '【入力IO】Insight検査グループ番号設定正常終了
56 MIN_IS_InspGSetNG%   =   11384      '【入力IO】Insight検査グループ番号設定異常終了
57 MIN_IS_InspOK%       =   11385      '【入力IO】Insight検査OK
58 MIN_IS_InspNG%       =   11386      '【入力IO】Insight検査NG
59 MIN_IS_InspErr%      =   11387      '【入力IO】Insight検査異常終了
60 MIN_IS_InspCapDone%  =   11388      '【入力IO】Insight検査画像取込完了
61 MIN_IS_ErrNum%       =   11408      '【入力IO】Insight処理エラー番号開始アドレス(16bit)
62 'Output Signal
63 MOUT_IS_JobLoadReq%  =   12370      '【出力IO】Insight JOBロード要求
64 MOUT_IS_InspGSetReq% =   12371      '【出力IO】Insight 検査グループ番号設定要求
65 MOUT_IS_Insp%        =   12372      '【出力IO】Insight 検査実行要求
66 MOUT_IS_JobNum%      =   12384      '【出力IO】Insight JOB番号設定開始アドレス(16bit)
67 MOUT_IS_InspGNum%    =   12400      '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
68 MOUT_InspErrNum%     =   12416      '【出力IO】検査実行エラー番号開始アドレス(16bit)
69 MOUT_InspNGStepNum%  =   12432      '【出力IO】検査実行NGStep番号開始アドレス(16bit)
70 '===== <電ドラ変数定義> =====
71 X20_Driver=11248                    '電ドラステイタス1　Driver Status 1
72 X21_Driver=11249 '電ドラステイタス2  Driver Status 2
73 X22_Driver=11250 '電ドラステイタス3  Driver Status 3
74 X23_Driver=11251 '電ドラステイタス4  Driver Status 4
75 X24_Driver=11252 '電ドラエラーメッセージ1 Driver Error E1
76 X25_Driver=11253 '電ドラエラーメッセージ2 Driver Error E2
77 X26_Driver=11254 '電ドラエラーメッセージ3 Driver Error E3
78 X27_Driver=11255 '電ドラエラーメッセージ4 Driver Error E4
79 X28_Driver=11256 '電ドラトータルエラーシグナル Total Error
80 X29_Driver=11257 '電ドラ終了シグナル Comlete signal
81 X2A_Driver=11258 '電ドラエラーメッセージ5 Driver Error E5
82 '11584   'toRBトルクドライバ-COMP_ERR送信
83 Y60_Driver=12240 '電ドラ半時計回り CCW
84 Y61_Driver=12241 '電ドラ時計回り CW
85 Y62_Driver=12242 'バンクセッティング BANK C1
86 Y63_Driver=12243 'バンクセッティング BANK C2
87 Y64_Driver=12244 'バンクセッティング BANK C3
88 Y65_Driver=12245 'プログラムセッティング PRG SET F1
89 Y66_Driver=12246 'プログラムセッティング PRG SET F2
90 Y67_Driver=12247 'プログラムセッティング PRG SET F3
91 X34_ScrewReady1=11259 'ねじっこ1　Read
92 '===== <電ドラ定数> =====
93 Dim PScrewPos(10)       'ネジ締め用Function引数変数
94 Dim PGetScrewPos(10)    'ねじ供給機からねじを得るFunction引数変数
95 Dim PEscapePosi(10)
96 MLoopCnt% = 0'
97 '===== <ロボット定数> =====
98 '===== <ロボット変数定義> =====
99 MRBTOpeGroupNo = 0      'ロボット動作番号初期化
100 MCommentD1001 = 0
101 MCommentD1002 = 0
102 MCommentD1003 = 0
103 MScreenNo = 0
104 '
105 MCommentTSU = 0
106 MCommentTSD = 0
107 'ウィンド画面番号設定
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
120 MClear% = 0        'KEY_のクリア
121 MAbout% = 1        'KEY_停止
122 MNext% = 2         'KEY_次のステップへ移行
123 MContinue% = 3     'KEY_継続 再度同じ動作を行う
124 '
125 MKeyNum% = 0       'KEY入力を受け取る
126 '
127 Def Inte MNgProcess
128 MNgProcess% = 5      'KEY_NG
129 '
130 MAssyOK% = 6       '組立完了
131 MPass% = 7         '工程パス
132 MPiasNG% = 8       'Pias確認時履歴NG
133 MIrregular% = 10   '例外処理実行用
134 '
135 '初期化用KEY番号   '
136 MRobotInit1% = 11  '初期位置用
137 MRobotInit2% = 12  '初期位置用
138 MRobotInit3% = 13  '初期位置用
139 MRobotInit4% = 14  '初期位置用
140 '
141 MIN_INIT1REQUEST% = 11568 'toRBT_ロボット初期位置1要求
142 MIN_INIT2REQUEST% = 11569 'toRBT_ロボット初期位置2要求
143 MIN_INIT3REQUEST% = 11570 'toRBT_ロボット初期位置3要求
144 MIN_INIT4REQUEST% = 11571 'toRBT_ロボット初期位置4要求
145 '
146 MOUT_INIT1RECIVE% = 12560 'toPLC_ロボット初期位置1受信
147 MOUT_INIT2RECIVE% = 12561 'toPLC_ロボット初期位置2受信
148 MOUT_INIT3RECIVE% = 12562 'toPLC_ロボット初期位置3受信
149 MOUT_INIT4RECIVE% = 12563 'toPLC_ロボット初期位置4受信
150 '
151 MOK% = 1               '各判定用
152 MNG% = 0               '各判定用
153 MTIMEOUT% = -1         '各判定用
154 MJudge% = 0            '判定情報格納用
155 '
156 MRECIVETIME& = 0
157 MSETTIMEOUT10& = 10000&                '10秒設定
158 MSETTIMEOUT08& = 8000&                 ' 8秒設定  2022/04/08 追加
159 MSETTIMEOUT03& = 3000&                 '3秒設定
160 MSETTIMEOUT01& = 1000&                 '1秒設定
161 MSETTIMEOUT05& = 5000&                 '5秒設定
162 MSETTIMEOUT009& = 900&                 '0.9秒設定
163 MSETTIMEOUT008& = 800&                 '0.8秒設定
164 MSETTIMEOUT007& = 700&                 '0.7秒設定
165 MSETTIMEOUT006& = 600&                 '0.6秒設定
166 MSETTIMEOUT005& = 500&                 '0.5秒設定
167 MSETTIMEOUT004& = 400&                 '0.4秒設定
168 MSETTIMEOUT003& = 300&                 '0.3秒設定
169 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
170 MIN_PIAS_ComOK% = 11552                'PC通信OK
171 MIN_PIAS_ComTimeOut% = 11576           'PC通信確認タイムアウト
172 MIN_PIAS_ComNG% = 11553                'PC通信NG
173 MOUT_PIAS_ComCheck% = 12544            'PC通信確認要求
174 MOUT_PIAS_Missing_Process% = 12546     '工程抜け確認要求
175 MIN_PIAS_ModelTypeNG% = 11554          'モデル仕向NG
176 MIN_PIAS_ProcessHistryNG% = 11555      '前工程履歴NG
177 MIN_PIAS_ProcessHistryOK% = 11556      '前工程履歴OK
178 MIN_PIAS_ProcessHistryErr% = 11557     '工程履歴処理エラー
179 MIN_PIAS_MyProcessComp% = 11573        '自工程履歴あり
180 MIN_PIAS_ProcessHistryTimeOut% = 11578 '工程履歴タイムアウト
181 MOUT_OKNG% = 12226                     'PLC OUT でOK=1, NG=0 出力
182 '
183 MOUT_PiasPCBNumberCheck = 12557        '基板番号照合
184 MIN_PiasPCBNumberOK% = 11566          '基板番号OK
185 MIN_PiasPCBNumberNG% = 11565          '基板番号NG
186 MIN_PiasPCBNumberErr% = 11567         '基板番号処理エラー
187 '
188 MOUT_PiasAssyResultOK% = 12549    '組立OK
189 MOUT_PiasAssyResultNG% = 12550    '組立NG
190 MOUT_PiasAssyResultWr% = 12548    '工程履歴書き込み
191 '
192 MIN_PiasProcessNG% = 11559        '工程履歴処理NG
193 MIN_PiasProcessOtherErr% = 11560  '工程履歴処理エラー(なんかのトラブル)
194 MIN_PiasProcessOK% = 11558        '工程履歴処理OK
195 '
196 MIN_Insight_Use% = 11374               '画像確認ON
197 MIN_TorqueCheck% = 11348               'トルクチェック
198 '
199 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT操作権
200 MOUT_RED_LIGHT% = 12356            'PATLIGHT 赤 点灯
201 MOUT_RED_FLASH% = 12357            'PATLIGHT 赤 点滅
202 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT 黄 点灯
203 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT 黄 点滅
204 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT 青 点灯
205 MOUT_GREEN_FLASH% = 12361          'PATLIGHT 青 点滅
206 '
207 MOUT_ST_DATETIME% = 12551          '組立開始日付時刻
208 MOUT_ED_DATETIME% = 12552          '組立終了日付時刻
209 '
210 MOUT_TORQUE_CHECK% = 12367         'PLCへトルクチェック中を送信
211 '
212 MIN_ASSY_CANCEL% = 11366           '組立を行うかのフラグ
213 '
214 MLoopFlg% = 0                      'KEY入力後のOK or NG内容
215 MopeNo% = 0
216 MRtn% = 0
217 MRet = 0
218 MRet3% = 0
219 '
220 Def Inte MInputQty          '投入数 演算変数
221 Def Inte MAssyOkQty         '組立ＯＫ数 演算変数
222 Def Inte MAssyNgQty         '組立ＮＧ数 演算変数(未使用)
223 Def Inte MSuctionErrQty     '吸着エラー数 演算変数 2022/04/27 渡辺
224 Def Inte nAssyOkQty         '未使用
225 Def Inte MScrewNo
226 Def Inte MReTry
227 '===== <IO変数定義> =====
228 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
229 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
230 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
231 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
232 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
233 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
234 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
235 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
236 '
237 Def Inte Y6A_VV1            ' アーム先端　ネジ吸着バルブ
238 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
239 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
240 '
241 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
242 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
243 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
244 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
245 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
246 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
247 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
248 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
249 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
250 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
251 '
252 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
253 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
254 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
255 '
256 Def Inte MOUT_LED1          ' 画像処理用LED照明
257 '
258 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
259 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
260 '
261 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
262 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
263 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
264 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
265 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
266 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
267 '
268 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
269 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
270 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
271 '
272 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
273 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
274 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
275 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
276 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
277 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
278 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
279 Y6A_VV1%    =  12250    ' アーム先端　ネジ吸着バルブ
280 Y6B_VB1%    =  12251    'アーム先端　吸着破壊バルブ
281 MOUT_VB1%   =  12251    ' アーム先端　ネジ吸着破壊バルブ
282 '
283 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
284 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
285 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
286 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
287 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
288 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
289 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
290 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
291 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
292 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
293 '
294 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
295 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
296 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
297 '
298 MOUT_LED1%  =  12239    ' 画像処理用LED照明
299 '
300 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
301 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
302 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
303 '
304 '共通
305 Def Inte MTEST_KEY                      'デバックテスト用
306 Def Inte MOn                            '出力=1
307 Def Inte MOff                           '出力=0
308 '
309 'ねじ締め装置_出力アドレス
310 Def Inte MOUT_ScwT_ComChk               '通信確認
311 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
312 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
313 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
314 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
315 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
316 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
317 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
318 'ねじ締め装置_入力アドレス
319 Def Inte MIN_ScwT_comOK                 '通信確認返信
320 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
321 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
322 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
323 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
324 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
325 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
326 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
327 '
328 Dim MScwT_Case1%(2)               '条件1停止変数
329 Dim MScwT_Case2%(2)               '条件2停止変数
330 Dim MScwT_Case3%(2)               '条件3停止変数
331 Dim MScwT_Case4%(2)               '条件4停止変数
332 Dim MScwT_Case5%(2)               '条件5停止変数
333 '
334 '共通
335 MTEST_KEY% = 11359                       'デバッグ用テストKEY
336 MOn% = 1                                 '出力 = 1
337 MOff% = 0                                '出力 = 0
338 '
339 'ねじ締め機_アドレス設定
340 MOUT_ScwT_ComChk% = 12832               '通信確認送信
341 MOUT_ScwT_ST% = 12865                   'ねじ締め開始を送信
342 MOUT_ScwT_ReSTOK% = 12866               '再開始受信を送信
343 MOUT_ScwT_FinOK% = 12868                'ねじ締め完了受信を送信
344 MOUT_ScwT_Case1OK% = 12858              '条件1停止受信を送信
345 MOUT_ScwT_Case2OK% = 12859              '条件2停止受信を送信
346 MOUT_ScwT_Case3OK% = 12860              '条件3停止受信を送信
347 MOUT_ScwT_Case4OK% = 12861              '条件4停止受信を送信
348 MOUT_ScwT_Case5OK% = 12862              '条件5停止受信を送信
349 '
350 MIN_ScwT_comOK% = 11840                 'ねじ締め装置から返信
351 MIN_ScwT_STRec% = 11873                 'ねじ締め開始を受信
352 MIN_ScwT_ReST% = 11874                  '再開始を受信
353 MIN_ScwT_Fin% = 11876                   'ねじ締め完了を受信
354 MIN_ScwT_Case1% = 11866                 '条件1停止待機を受信
355 MIN_ScwT_Case2% = 11867                 '条件2停止待機を受信
356 MIN_ScwT_Case3% = 11868                 '条件3停止待機を受信
357 MIN_ScwT_Case4% = 11869                 '条件4停止待機を受信
358 MIN_ScwT_Case5% = 11870                 '条件5停止待機を受信
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
371 '設定 InitialZoneBで使用する変数
372 Def Pos PActive       '直交座標系 位置変数 現在位置
373 Def Pos Pmove         '直交座標系 位置変数 移動先
374 Def Jnt JActive       '関節座標系 位置変数 現在位置
375 Def Jnt Jmove         '関節座標系 位置変数 移動先
376 Def Jnt JTaihi        '関節座標系 位置変数 退避ポジション ティーチングで設定
377 Def Inte MRecoveryPass      '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行
378 Def Inte MStandby      '待機位置確認フラグ
379 Def Inte MRecoveryChuckOpen    'チャック解放フラグ（復帰動作前）両掴み対策
380 '★注意★初期位置を変更した時には、変更が必要！
381 '
382 '
383 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
384 Function M% fnAssyStart
385     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
386 ' PIASチケット読込み工程抜け確認
387     M_20# = MClear%                       '初期化
388     If M_22# = MIrregular% Then GoTo *IRREGULAR     'Assy完了後DVDメカを把持している場合
389     '開始位置がイニシャルポジションだった場合(ハンド交換治具が無いかチェック)
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
400     'イニシャルポジションにいた場合センサーチェック
401     If MRtn = 1 Then
402         Ovrd 20
403         Accel 100 , 20
404         Mvs PHandChange                             'ハンド交換位置に移動
405         Ovrd 100
406         Accel 100 , 100
407         MRtn = frInCheck(11264,0,MSETTIMEOUT03&)    'センサーチェック(治具が無いか)
408         Mvs PInitialPosition                        'イニシャルポジションに移動
409     Else
410         MRtn = 1
411     EndIf
412     If MRtn = 1 Then GoTo *AssyStart
413     fErrorProcess(11,286,287,0)                        'ハンド交換用治具がある
414     If M_20# = MNext% Then M_20# = MClear%
415     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
416     If M_20# = MNgProcess% Then GoTo *RE_START
417     If M_20# = MContinue% Then GoTo *RE_START
418 '
419     *AssyStart
420 '
421 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
422 '        MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
423 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
424 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
425 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' メニューへ戻る
426 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NGを工程履歴に書込み次の工程へ
427 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NGを工程履歴に書込み次の工程へ
428 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' 履歴NG, 工程抜け
429 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK以外は組立終了
430 '    EndIf
431 ' ネジ締め機テスト用 ----------
432 '    'Mret% = fScewTcomChk()
433 '    'ねじ締め開始
434     MRtn2 = fScewTStart()
435 '    '
436 '    '座標移動
437 '    '
438 '    '条件xx停止
439 '    fScewTCaseStop(MScwT_Case5%)
440 '    '
441 '    'ベースユニットKEY
442 '    Wait M_In(MTEST_KEY%) = MOn%
443 '    '
444 '    '再開始
445 '    fScewTReStart()
446 '    '
447 '    '座標移動
448 '    '
449 '    'ねじ締め完了
450 '    Mret% = fScewTFinish()
451 ' ネジ締めテスト終了
452 ' PIASテスト -----------
453 '    MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
454 '    MRet% = fnPiasWrite(MNG%)
455  '   MRet% = fnPCBNumberCheck()     'デバック用にコメントアウト(9/17中村)
456 ' PIASテスト終了 -------
457 '組み立て開始(仮組み9/10中村)
458 'プログラム原点
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
473 'ハンドにDVDメカ,ブラケットが無いか
474 '
475 *INITIAL_CHECK
476 '
477 If M_In(11264) = 0 And M_In(11267) = 0 And M_In(11270) = 0 Then GoTo *CompInitial1  'DVDメカ,ブラケットが無いか
478 fErrorProcess(11,253,287,0)                 '284→287に変更6/3中村
479 If M_20# = MNext% Then M_20# = MClear%
480 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
481 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
482 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
483 *CompInitial1
484 '
485 'ハンドをイニシャルに戻す
486 If M_In(11266) = 1 Then     'DVDチャック閉検出
487     M_Out(12256) = 0        'DVDチャック閉OFF
488     M_Out(12257) = 1        'DVDチャック開ON
489     Break
490 EndIf
491 If M_In(11269) = 1 Then     'Fシリンダー出検出
492     M_Out(12258) = 0        'Fシリンダー出OFF
493     M_Out(12259) = 1        'Fシリンダー戻ON
494     Break
495 EndIf
496 If M_In(11272) = 1 Then     'Rシリンダー出検出
497     M_Out(12260) = 0        'Rシリンダー出OFF
498     M_Out(12261) = 1        'Rシリンダー戻ON
499     Break
500 EndIf
501 '
502 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    'DVDチャック開検出
503 If MRtn = 1 Then GoTo *CompInitial2
504 fErrorProcess(11,270,284,0)
505 If M_20# = MNext% Then M_20# = MClear%
506 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
507 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
508 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
509 *CompInitial2
510 '
511 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)    'Fシリンダー戻検出
512 If MRtn = 1 Then GoTo *CompInitial3
513 fErrorProcess(11,278,284,0)
514 If M_20# = MNext% Then M_20# = MClear%
515 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
516 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
517 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
518 *CompInitial3
519 '
520 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)    'Rシリンダー戻検出
521 If MRtn = 1 Then GoTo *CompInitial4
522 fErrorProcess(11,276,284,0)
523 If M_20# = MNext% Then M_20# = MClear%
524 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
525 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
526 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
527 *CompInitial4
528 '
529 ' 2022/04/11 安全方向へ条件追加 渡辺
530 ' PInitialPosition 在席 MStandby=1
531 ' PMechaOnJigGet_3 在席 MStandby=2
532 '
533 MStandby = 0    '待機位置フラグを初期化
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
557 fErrorProcess(11,230,281,0)          '初期位置にいない時はエラーにする
558 If M_20# = MNext% Then GoTo *ASSY_ERROR_END
559 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
560 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
561 If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
562 *PositionOK
563 '
564 '
565 'DVD無しPASSプログラムから通常プログラムに切替えた時の対策 2022.05.13 渡辺
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
578 M_Out(12912) = 1                  'Ver 0.4 追加　仮置き台フラグ立て(工程6優先のため工程5のフラグ監視の前に出力)
579 Mov PMechaOnJigGet_3    'タクト短縮のため初期位置変更
580 'Wait M_In(11920) = 0              'BaseUnit5仮置き台フラグ確認(処理位置移動2/9中村)
581 M_Out(12912) = 1                  '仮置き台フラグ立て(念のため追加2/9中村)
582 'Ver 0.4 追加--------------------
583 If M_In(11920) = 1 Then GoTo *CompInitial4         '工程6が動作中の場合11920 = 1 ループ
584 'Ver 0.4 ここまで----------------           '工程6優先のため12912=0にしない
585 '
586 'Mov PInitialPosition   '1/20コメントアウト(中村)
587 '
588 '仮置き台回避点へ移動
589 Mov PMechaOnJigGet_2         '仮置き台回避点
590 '
591 '仮置き台からDVDメカを受け取る
592 'Wait M_In(12914) = 1              '供給許可を受信
593 'Wait M_In(11920) = 0              'BaseUnit5仮置き台フラグ確認(追加ここから10/1中村)
594 '
595     MRtn2 = 0                   'エラー時次へが押された時用
596 *RE_JIG_GET
597     M_20# = MClear%
598 '
599     M_Out(12912) = 1                  '仮置き台フラグ立て
600     If M_In(11278) = 1 Then          '方向の確認(CW端センサー)(追加ここまで10/1中村)
601         Mov PMechaOnJigGet1_1    '仮置き台上空
602         Ovrd 25
603         Mvs PMechaOnJigGet1      'DVDメカ受け取り位置
604         M_Out(12257) = 0         'DVDメカチャック開OFF
605         M_Out(12256) = 1         'DVDメカチャック閉ON
606 '        Wait M_In(11266) = 1     'DVDメカチャック閉検出
607         MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
608         If MRtn = 0 And MRtn2 = 0 Then             'DVDメカを把持できなかった場合チャックを開く(6/3中村)
609             M_Out(12256) = 0         'DVDメカチャック閉OFF
610             M_Out(12257) = 1         'DVDメカチャック開ON
611         EndIf
612         Mvs PMechaOnJigGet1_1    '仮置き台上空
613         Ovrd 100
614         Break
615     ElseIf M_In(11277) = 1 Then       '方向の確認(CCW端センサー)(追加ここから10/1中村)
616         Mov PMechaOnJigGet2_1    '仮置き台上空
617         Ovrd 25
618         Mvs PMechaOnJigGet2      'DVDメカ受け取り位置
619         M_Out(12257) = 0         'DVDメカチャック開OFF
620         M_Out(12256) = 1         'DVDメカチャック閉ON
621 '        Wait M_In(11266) = 1     'DVDメカチャック閉検出
622         MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
623         If MRtn = 0 And MRtn2 = 0 Then             'DVDメカを把持できなかった場合チャックを開く(6/3中村)
624             M_Out(12256) = 0         'DVDメカチャック閉OFF
625             M_Out(12257) = 1         'DVDメカチャック開ON
626         EndIf
627         Mvs PMechaOnJigGet2_1    '仮置き台上空
628         Ovrd 100
629         Break
630     EndIf
631     '
632     If MRtn = 1 Or MRtn2 = 1 Then GoTo *CompMechaGet1    'DVDメカチャックエラー処理(Or MRtn2 = 1追加6/3中村)
633 '    PTemp = P_Curr              '処理位置の変更1/12中村
634 '    Mvs PTemp , -150
635     Mov PMechaOnJigGet_2
636     fErrorProcess(11,269,294,0) '284→294に変更6/3中村
637 '    PTemp = P_Curr             '処理位置の変更1/12中村
638 '    Mvs PTemp , -150
639 '    Mov PMechaOnJigGet_2
640     If M_20# = MNext% Then
641         M_20# = MClear%
642         MRtn2 = 1           '追加6/3中村
643     EndIf
644     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '追加6/3中村
645         Mov PInitialPosition
646         Break
647     EndIf
648     If M_20# = MContinue% Then MRtn2 = 0
649     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition '追加6/3中村
650     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
651     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
652     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_JIG_GET  '次へが押された場合も戻るように変更(6/3中村)
653 *CompMechaGet1
654 '
655     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
656 Mov PMechaOnJigGet_2            '仮置き台回避点
657     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
658 '
659 'DVDメカセンサー検出
660 '
661 'M_Out(12912) = 0                  '仮置き台フラグ回収(処理位置変更2/9中村)
662 '
663 '
664 'DVDメカをねじロボ3に置く
665 Mov PMechaOnRoboSet_2        'ねじロボ回避点
666 '
667 'Wait M_In(11888) = 1         'ねじロボ3停止1まで待機
668 MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
669 If MRtn = 0 Then Mov PInitialPosition     '"イニシャルに戻る動き"
670 If MRtn = 0 Then GoTo *ASSY_ERROR_END
671 '
672 Mov PMechaOnRoboSet_1        'ねじロボ上空
673 Ovrd 25
674 Mvs PMechaOnRoboSet          'DVDメカ置き場
675 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止1～停止2まで)
676 '
677 'Wait M_In(11889) = 1         'ねじロボ3停止2まで待機
678 MRtn = fScrewTighenRoboCheck(11889)    '停止状態を受信する
679 'If MRtn = 0 Then
680 '    M_Out(12256) = 0             'DVDメカチャック閉OFF
681 '    M_Out(12257) = 1             'DVDメカチャック開ON
682 '    Mvs PMechaOnRoboSet_1
683 '    Mov PMechaOnRoboSet_2
684 '    Mov PInitialPosition
685 'EndIf
686 If MRtn = 0 Then GoTo *ASSY_ERROR_END   'その場で停止処理(故障の可能性)
687 '
688 *RE_ROBO_SET_1
689 '
690 M_Out(12256) = 0             'DVDメカチャック閉OFF
691 M_Out(12257) = 1             'DVDメカチャック開ON
692 '
693 'Wait M_In(11265) = 1         'DVDメカチャック開検出
694 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
695 If MRtn = 1 Then GoTo *CompRoboSet1
696 fErrorProcess(11,270,284,0)
697 If M_20# = MNext% Then M_20# = MClear%
698 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
699 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
700 If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
701 *CompRoboSet1
702 '
703 '    ' 部品供給要求送信(処理位置変更1/20中村)
704     M_Out(12787) = 1
705 '
706 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止2～停止3まで)
707 '
708 'Wait M_In(11890) = 1         'ねじロボ3停止3まで待機
709 MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する
710 MScrewRoboNgFlg% = 0
711 If MRtn = 0 Then MScrewRoboNgFlg% = 1
712 '
713 Mvs PMechaOnRoboSet_1        'ねじロボ上空
714 Ovrd 100
715 Mov PMechaOnRoboSet_2        'ねじロボ回避点
716 M_Out(12912) = 0                  '仮置き台フラグ回収(処理位置変更2/9中村)
717 '
718 '
719 If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition   'ねじロボで停止かNGが押されていた場合
720 If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END   '
721 '
722 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止3～停止4まで)
723 '
724 ''    ' 部品供給要求送信(処理位置位置変更1/20中村)
725 '    M_Out(12787) = 1
726 '    '    ' 部品供給完了待ち
727 '    Wait M_In(11810) = 1
728 '
729 'DVD金具(R)を取る
730 *RE_R_GET_3
731 Mov PBracketRGet_3           '経路
732 Mov PBracketRGet_2           '金具(R)受け取り回避点
733 M_Out(12261) = 0             '金具(R)シリンダー戻OFF
734 M_Out(12260) = 1             '金具(R)シリンダー出ON
735 '
736     '    ' 部品供給完了待ち(処理変更2/27中村)
737 *RE_FEEDER_READY
738 '    Wait M_In(11810) = 1
739     fnAutoScreenComment(513)    '状態表示[部品供給待ち] 2022/04/26 渡辺
740 'MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '供給待ち
741 MRtn = frInCheck(11810,1,MSETTIMEOUT08&)   '供給待ち  2022/04/07 変更
742 If MRtn = 1 Then GoTo *CompFeederReady
743 '    ' 部品供給要求終了
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
755 '    ' 部品供給要求
756     M_Out(12787) = 1
757 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
758 *CompFeederReady
759 '    ' 部品供給要求終了
760     M_Out(12787) = 0
761 '
762     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
763 Mov PBracketRGet_1           '金具(R)受け取り上空
764 '
765 *RE_R_GET_1
766 '
767 If M_20# = MContinue% Then
768     M_Out(12261) = 0             '金具(R)シリンダー戻OFF
769     M_Out(12260) = 1             '金具(R)シリンダー出ON
770     M_20# = MClear%
771 EndIf
772 '
773 'Wait M_In(11272) = 1         '金具(R)シリンダー出端検出
774 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '金具(R)シリンダー出端検出
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
787 Mvs PBracketRGet             '金具(R)受け取り位置
788 '
789 '
790 M_Out(12252) = 0             '真空OFFバルブOFF
791 M_Out(12253) = 0             '真空破壊バルブOFF(念のため)
792 M_Out(12251) = 1             '真空ONバルブON
793 '
794 'Wait M_In(11270) = 1         '金具(R)吸着センサーON検出
795 MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   '金具(R)吸着センサーON検出
796 If MRtn = 1 Then GoTo *CompRGet2
797 Mvs PBracketRGet_1           '金具(R)受け取り上空
798 fErrorProcess(11,279,295,0)  '284→295へ変更6/3中村
799 If M_20# = MNext% Then M_20# = MClear%
800 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
801 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
802 If M_20# = MContinue% Then GoTo *RE_R_GET_2
803 *CompRGet2
804 '
805 Mvs PBracketRGet_1           '金具(R)受け取り上空
806 Ovrd 100
807 Mov PBracketRGet_2           '金具(R)受け取り回避点
808 Mov PBracketRGet_3           '経路
809 '
810 'DVD金具(R)を置く
811 Mov PBracketRSet_3           '回避点
812 MRtn = frInCheck(11270,1,MSETTIMEOUT05&)              'もう一度金具(R)吸着センサーON検出
813 If MRtn = 1 Then GoTo *CompRGet3
814 fErrorProcess(11,279,295,0)  '284→295へ変更6/3中村
815 If M_20# = MNext% Then M_20# = MClear%
816 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
817 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
818 If M_20# = MContinue% Then GoTo *RE_R_GET_3
819 *CompRGet3
820 '
821 'Wait M_In(11891) = 1         'ねじロボ3停止4まで待機
822 MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
823 If MRtn = 0 Then Mov PInitialPosition
824 If MRtn = 0 Then GoTo *ASSY_ERROR_END
825 '
826 Mov PBracketRSet_2           '金具(R)置き位置回避点
827 Mvs PBracketRSet_1           '金具(R)置き位置上空
828 *RE_R_SET_1                  '戻し位置変更6/3中村
829 Ovrd 10
830 Mvs PBracketRSet             '金具(R)置き位置
831 Dly 0.2
832 '
833 '*RE_R_SET_1                  '戻し位置変更6/3中村
834 '
835 M_Out(12251) = 0             '真空ONバルブOFF
836 M_Out(12252) = 1             '真空OFFバルブON
837 M_Out(12253) = 1             '真空破壊バルブON
838 '
839 MRtn = frInCheck(11270,0,MSETTIMEOUT05&)
840 '
841 Dly 0.5
842 'M_Out(12253) = 0             '真空破壊バルブOFF
843 '
844 If MRtn = 1 Then GoTo *CompRSet1
845 Mvs PBracketRSet_1           '金具(R)置き位置上空
846 M_Out(12253) = 0             '真空破壊バルブOFF
847 fErrorProcess(11,296,297,0)  '236,284→296,297に変更6/2中村
848 If M_20# = MNext% Then M_20# = MClear%
849 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
850 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
851 If M_20# = MContinue% Then GoTo *RE_R_SET_1
852 *CompRSet1
853 '
854 'M_Out(12260) = 0             '金具(R)シリンダー出OFF(戻し位置変更1/20中村)
855 'M_Out(12261) = 1             '金具(R)シリンダー戻ON
856 ''
857 '*RE_R_SET_2
858 ''
859 ''Wait M_In(11271) = 1         '金具(R)シリンダー戻端検出
860 'MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   '金具(R)シリンダー戻端検出
861 'If MRtn = 1 Then GoTo *CompRSet2
862 'fErrorProcess(11,276,284,0)
863 'If M_20# = MNext% Then M_20# = MClear%
864 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
865 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
866 'If M_20# = MContinue% Then GoTo *RE_R_SET_2
867 '*CompRSet2
868 '
869 Ovrd 1                       '引っかかり防止(5/13中村)
870 Mvs PBracketRSet , -5        '引っかかり防止(5/13中村)
871 Ovrd 100                     '引っかかり防止(5/13中村)
872 Mvs PBracketRSet_1           '金具(R)置き位置上空
873 M_Out(12253) = 0             '真空破壊バルブOFF
874 Ovrd 100
875 Mov PBracketRSet_2           '金具(R)置き位置回避点
876 Mov PBracketRSet_3           '回避点
877 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止4～停止5まで)
878 '
879 *RE_R_SET_2
880 '
881 M_Out(12260) = 0             '金具(R)シリンダー出OFF(戻し位置変更1/20中村)
882 M_Out(12261) = 1             '金具(R)シリンダー戻ON
883 '
884 'Wait M_In(11271) = 1         '金具(R)シリンダー戻端検出
885 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   '金具(R)シリンダー戻端検出
886 If MRtn = 1 Then GoTo *CompRSet2
887 fErrorProcess(11,276,284,0)
888 If M_20# = MNext% Then M_20# = MClear%
889 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
890 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
891 If M_20# = MContinue% Then GoTo *RE_R_SET_2
892 *CompRSet2
893 '
894 '
895 '置き位置画像検査(処理位置移動)
896 'Wait M_In(11892) = 1         'ねじロボ3停止5まで待機(画像検査からF側ブラケット置き)
897 'MRtn = fScrewTighenRoboCheck(11892)    '停止状態を受信する
898 'If MRtn = 0 Then Mov PInitialPosition
899 'If MRtn = 0 Then GoTo *ASSY_ERROR_END
900 '
901 'Wait M_In(11920) = 0              'BaseUnit5仮置き台フラグ確認(追加ここから10/1中村)
902 ''
903 'M_Out(12912) = 1                  '仮置き台フラグ立て(衝突防止)
904 ''
905 ''Mov PBracketRCheck_2         '経路
906 ''Mov PBracketRCheck           '検査位置
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
928 'M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止5～停止6まで)
929 '
930 'Mov PBracketRCheck
931 '
932 'DVD金具(F)を取る
933 *RE_F_GET_3
934 'M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止5～停止6まで)(処理位置変更1/20中村)
935 Mov PBracketFGet_3           '回避点
936 '
937 M_Out(12912) = 0                  '仮置き台フラグ回収(衝突防止)
938 '
939 Mov PBracketFGet_2           '金具(F)受け取り回避点
940 'Mov PBracketFGet_1           '金具(F)受け取り上空(移動タイミング変更1/20中村)
941 '
942 *RE_F_GET_1
943 '
944 M_Out(12259) = 0             '金具(F)シリンダー戻OFF
945 M_Out(12258) = 1             '金具(F)シリンダー出ON
946 '
947 Mov PBracketFGet_1           '金具(F)受け取り上空(移動タイミング変更1/20中村)
948 '
949 'Wait M_In(11269) = 1         '金具(F)シリンダー出端検出
950     fnAutoScreenComment(513)    '状態表示[部品供給待ち] 2022/04/26 渡辺
951 MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   '金具(F)シリンダー出端検出
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
962     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
963 Mvs PBracketFGet             '金具(F)受け取り位置
964 '
965 '
966 '
967 M_Out(12249) = 0             '真空OFFバルブOFF
968 M_Out(12250) = 0             '真空破壊バルブOFF
969 M_Out(12248) = 1             '真空ONバルブON
970 '
971 'Wait M_In(11267) = 1         '金具(F)吸着センサーON検出
972 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '金具(F)吸着センサーON検出
973 If MRtn = 1 Then GoTo *CompFGet2
974 Mvs PBracketFGet_1           '金具(F)受け取り上空
975 fErrorProcess(11,280,295,0)  '284→295に変更6/3中村
976 If M_20# = MNext% Then M_20# = MClear%
977 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
978 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
979 If M_20# = MContinue% Then GoTo *RE_F_GET_2
980 *CompFGet2
981 '
982 Mvs PBracketFGet_1           '金具(F)受け取り上空
983 'Ovrd 100
984 Mov PBracketFGet_2           '金具(F)受け取り回避点
985 Mov PBracketFGet_3           '回避点
986 '
987 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)          'もう一度金具(F)吸着センサーON検出
988 If MRtn = 1 Then GoTo *CompFGet3
989 fErrorProcess(11,280,295,0)  '284→295に変更6/3中村
990 If M_20# = MNext% Then M_20# = MClear%
991 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
992 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
993 If M_20# = MContinue% Then GoTo *RE_F_GET_3
994 *CompFGet3
995 '
996 'DVD金具(F)を置く
997 Mov PBracketFSet_3           '回避点
998 '    ' 部品供給要求終了
999     M_Out(12787) = 0
1000 '    ' 部品取得完了送信(パルス)
1001     M_Out(12800) = 1 Dly 0.5
1002     '
1003 Ovrd 70
1004 Mov PBracketFSet_2           '金具(F)置き位置回避点
1005 'Wait M_In(11893) = 1         'ねじロボ3停止5まで待機
1006 MRtn = fScrewTighenRoboCheck(11892)    '停止状態を受信する
1007 If MRtn = 0 Then Mov PInitialPosition1  '停止位置に移動
1008 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1009 '
1010 '    fnAutoScreenComment(533)    '状態表示[工程５のロボ動作終了待ち] 2022/04/26 渡辺(コメントアウト5/13中村)
1011 'Wait M_In(11920) = 0              'BaseUnit5仮置き台フラグ確認(追加ここから10/1中村)(コメントアウト5/13中村)
1012 ''
1013 'M_Out(12912) = 1                  '仮置き台フラグ立て(衝突防止)(コメントアウト5/13中村)
1014 ''
1015     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1016 'Ovrd 70
1017 'Mov PBracketFSet_2           '金具(F)置き位置回避点
1018 Mvs PBracketFSet_1           '金具(F)置き位置上空
1019 *RE_F_SET_1                  '戻し位置変更6/3中村
1020 Ovrd 25
1021 Mvs PBracketFSet             '金具(F)置き位置
1022 Dly 0.1
1023 '
1024 '*RE_F_SET_1                  '戻し位置変更6/3中村
1025 '
1026 M_Out(12248) = 0             '真空ONバルブOFF
1027 M_Out(12249) = 1             '真空OFFバルブON
1028 M_Out(12250) = 1             '真空破壊バルブON
1029 '
1030 MRtn = frInCheck(11267,0,MSETTIMEOUT05&)
1031 Dly 0.5
1032 'M_Out(12250) = 0             '真空破壊バルブOFF
1033 '
1034 If MRtn = 1 Then GoTo *CompFSet1
1035 M_Out(12250) = 0             '真空破壊バルブOFF
1036 fErrorProcess(11,296,297,0)  '236,284→296,297に変更6/2中村
1037 If M_20# = MNext% Then M_20# = MClear%
1038 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1039 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1040 If M_20# = MContinue% Then GoTo *RE_F_SET_1
1041 *CompFSet1
1042 '
1043 '
1044 '
1045 '*RE_F_SET_2'(戻し位置変更)
1046 ''
1047 'M_Out(12258) = 0             '金具(F)シリンダー出OFF
1048 'M_Out(12259) = 1             '金具(F)シリンダー戻ON
1049 ''
1050 ''Wait M_In(11268) = 1         '金具(F)シリンダー戻端検出
1051 'MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '金具(F)シリンダー戻端検出
1052 'If MRtn = 1 Then GoTo *CompFSet2
1053 'fErrorProcess(11,277,284,0)
1054 'If M_20# = MNext% Then M_20# = MClear%
1055 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1056 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1057 'If M_20# = MContinue% Then GoTo *RE_F_SET_2
1058 '*CompFSet2
1059 '
1060 Ovrd 1                       'ブラケット引っかかり防止(5/12中村)
1061 Mvs PBracketFSet , -5        'ブラケット引っかかり防止(5/12中村)
1062 Ovrd 100
1063 Mvs PBracketFSet_1           '金具(F)置き位置上空
1064 M_Out(12250) = 0             '真空破壊バルブOFF
1065 Mvs PBracketFSet_2           '金具(F)置き位置回避点
1066 Mov PBracketFSet_3           '回避点
1067 M_Out(12912) = 0             '仮置き台フラグ回収
1068 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止5～停止6まで)
1069 *RE_F_SET_2'(戻し位置変更)
1070 '
1071 M_Out(12258) = 0             '金具(F)シリンダー出OFF
1072 M_Out(12259) = 1             '金具(F)シリンダー戻ON
1073 '
1074 'Wait M_In(11268) = 1         '金具(F)シリンダー戻端検出
1075 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '金具(F)シリンダー戻端検出
1076 If MRtn = 1 Then GoTo *CompFSet2
1077 fErrorProcess(11,277,284,0)
1078 If M_20# = MNext% Then M_20# = MClear%
1079 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1080 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1081 If M_20# = MContinue% Then GoTo *RE_F_SET_2
1082 *CompFSet2
1083 '
1084 '----------PIAS先読み----------
1085 '概要：読みに行くかどうかを決めるときにタイムアウトで先に進めるようにするタイマー
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
1096         ElseIf MCrtTime& > 5000 Then    '暫定5秒(分割しているのは秒数調整しやすくしている為)
1097             MloopFlg = 1
1098         EndIf
1099     WEnd
1100     MRtn = 0
1101     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
1102         If M_In(11354) = 1 Then         '組立開始がONなら
1103             M_Out(12346) = 1 Dly 0.5        ' 組立開始を受信
1104             Mvs PTicketRead
1105             MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
1106 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1107 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
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
1120 '置き位置画像検査
1121 'Wait M_In(11893) = 1         'ねじロボ3停止6まで待機
1122 MRtn = fScrewTighenRoboCheck(11893)    '停止状態を受信する
1123 If MRtn = 0 Then Mov PInitialPosition1  '停止位置に移動
1124 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1125 '
1126 If M_In(11369) = 0 Then M_Out(12912) = 1    '画像判定未使用時ここでフラグ立て6/23中村
1127 If M_In(11369) = 0 Then GoTo *SkipCheck1    '画像判定未使用時ジャンプ
1128 *RE_FLG_SET_1
1129 'Wait M_In(11920) = 0                'BaseUnit5仮置き台フラグ確認(追加2/27中村)
1130 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5仮置き台フラグ確認(タイムアウト処理を追加22/09/29中村)
1131 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1132 If MRtn = 2 Then GoTo *RE_FLG_SET_1
1133 M_Out(12912) = 1                    '仮置き台フラグ立て
1134 'Wait M_In(11920) = 0              'Ver 0.4 追加
1135 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5仮置き台フラグ確認(タイムアウト処理を追加22/09/29中村)
1136 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1137 If MRtn = 2 Then GoTo *RE_FLG_SET_1
1138 Dly 0.3
1139 'If M_In(11920) = 1 Then M_Out(12912) = 0     'Ver 0.4 コメントアウト　工程6優先
1140 If M_In(11920) = 1 Then GoTo *RE_FLG_SET_1   '
1141 '
1142 'Wait M_In(11920) = 0              'BaseUnit5仮置き台フラグ確認(追加ここから10/1中村)
1143 ''
1144 'M_Out(12912) = 1                  '仮置き台フラグ立て(衝突防止)
1145 '
1146 'Mov PBracketFCheck_2         '経路
1147 'Mov PBracketFCheck           '検査位置
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
1174 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止6～停止7まで)
1175 If M_In(11369) = 1 Then Mov PBracketRCheck1
1176 'Wait M_In(11894) = 1         'ねじロボ3停止7まで待機
1177 MRtn = fScrewTighenRoboCheck(11894)    '停止状態を受信する
1178 If MRtn = 0 Then
1179     Mov PBracketFCheck
1180     Mov PInitialPosition
1181     M_Out(12912) = 0
1182 EndIf
1183 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1184 '
1185 If M_In(11369) = 0 Then GoTo *SkipCheck2    '画像判定未使用時ジャンプ
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
1210 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止7～停止8まで)処理位置変更1/20中村
1211 '
1212 If M_In(11369) = 1 Then Mov PBracketFCheck
1213 '
1214 '
1215 'ねじロボ3のDVDassyを取る
1216 'M_Out(12866) = 1 Dly 0.5                   'ねじロボ3に再開要求(停止7～停止8まで)処理位置変更1/20中村
1217 Mov PMechaOnRoboGet_3                       '向き合わせ
1218 'M_Out(12912) = 0                            '仮置き台フラグ回収(6/23暫定コメントアウト(中村))
1219 '
1220 Mov PMechaOnRoboGet_2                       '回避点(処理位置変更1/20中村)
1221 '
1222 'Wait M_In(11895) = 1                       'ねじロボ3停止8まで待機
1223 MRtn = fScrewTighenRoboCheck(11895)         '停止状態を受信する
1224 If MRtn = 0 Then Mov PInitialPosition1      '停止位置に移動
1225 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1226 '
1227 *RE_FLG_SET_2
1228     fnAutoScreenComment(533)    '状態表示[工程５のロボ動作終了待ち] 2022/04/26 渡辺
1229 'Wait M_In(11920) = 0                'BaseUnit5仮置き台フラグ確認(追加2/27中村)
1230 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5仮置き台フラグ確認(タイムアウト処理を追加22/09/29中村)
1231 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1232 If MRtn = 2 Then GoTo *RE_FLG_SET_2'
1233 M_Out(12912) = 1                            '仮置き台フラグ立て(衝突防止)
1234 Dly 0.3
1235 'Wait M_In(11920) = 0                        'Ver 0.4 追加　工程6優先
1236 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5仮置き台フラグ確認(タイムアウト処理を追加22/09/29中村)
1237 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1238 If MRtn = 2 Then GoTo *RE_FLG_SET_2
1239 'If M_In(11920) = 1 Then M_Out(12912) = 0   'Ver 0.4 コメントアウト
1240 If M_In(11920) = 1 Then GoTo *RE_FLG_SET_2
1241 '
1242 'Mov PMechaOnRoboGet_2                      '処理位置変更(1/20中村)
1243 Mov PMechaOnRoboGet_1                       'ねじロボ上空
1244 Ovrd 25
1245 '
1246 *RE_ROBO_GET_1
1247 '
1248     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1249 Mvs PMechaOnRoboGet          'DVDメカ受け取り位置
1250 Dly 0.1
1251 M_Out(12257) = 0             'DVDチャック開OFF
1252 M_Out(12256) = 1             'DVDチャック閉ON
1253 '
1254 'Wait M_In(11266) = 1         'DVDメカチャック閉検出
1255 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
1256 If MRtn = 1 Then GoTo *CompRoboGet1
1257 fErrorProcess(11,269,284,0)
1258 If M_20# = MNext% Then M_20# = MClear%
1259 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1260 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1261 If M_20# = MContinue% Then GoTo *RE_ROBO_GET_1
1262 *CompRoboGet1
1263 '
1264 M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止8～停止9まで)
1265 '
1266 'Wait M_In(11876) = 1         'ねじロボ3ねじ締め完了を受信
1267 '
1268 'Wait M_In(11896) = 1         'ねじロボ3停止9まで待機
1269 MRtn = fScrewTighenRoboCheck(11876)    '停止状態を受信する
1270 'If MRtn = 0 Then
1271 '    M_Out(12256) = 0             'DVDメカチャック閉OFF
1272 '    M_Out(12257) = 1             'DVDメカチャック開ON
1273 '    Mvs PMechaOnRoboGet_1
1274 '    Mov PMechaOnRoboGet_2
1275 '    Mov PInitialPosition
1276 'EndIf
1277 If MRtn = 0 Then GoTo *ASSY_ERROR_END   'その場で停止
1278 '
1279 'Wait M_In(11264) = 1         'DVDメカ検出
1280 *RE_ROBO_GET_2
1281 MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   'DVDメカ検出
1282 If MRtn = 1 Then GoTo *CompRoboGet2
1283 fErrorProcess(11,273,284,0)
1284 If M_20# = MNext% Then M_20# = MClear%
1285 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1286 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1287 If M_20# = MContinue% Then GoTo *RE_ROBO_GET_2
1288 *CompRoboGet2
1289 '
1290 Mvs PMechaOnRoboGet_1        'ねじロボ上空
1291 Ovrd 100
1292 Mov PMechaOnRoboGet_2        'ねじロボ回避点
1293 'M_Out(12866) = 1 Dly 0.5     'ねじロボ3に再開要求(停止9～ねじ締め完了まで)
1294 M_Out(12912) = 0                  '仮置き台フラグ回収(追加10/1中村)
1295 '
1296 Mov PTicketRead_2
1297 M_Out(12868) = 1 Dly 0.5     'ねじロボ3ねじ締め完了を送信(処理位置変更3/26中村)
1298 '
1299     If M_22# = MAssyOK% Then GoTo *CompRead
1300 *IRREGULAR      '開始時Assy完了後DVDメカ把持ジャンプ先
1301     Mov PTicketRead_1
1302 '
1303 'DVDassyをパレットへ置く
1304 '    Wait M_In(11218) = 1         'パレットが上昇していることを確認
1305     If M_22# <> MClear% And M_22# <> MIrregular% Then GoTo *RE_PIAS_CHECK
1306     fnAutoScreenComment(95)    '状態表示[パレット搬送待機中] 2022/04/26 渡辺
1307     Wait M_In(11354) = 1         ' 組立開始信号が出ていることを確認
1308     M_Out(12346) = 1 Dly 0.5         ' 組立開始を受信
1309     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1310 *RE_PIAS_CHECK
1311     M_20# = MClear%                 '初期化
1312     MRtn = 1
1313     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
1314         If M_22# = MClear% Or M_22# = MContinue% Or M_22# = MIrregular% Then'PIASチェックにて未検査かリトライ時に入る
1315                 Mvs PTicketRead
1316                 MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
1317 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1318 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1319         EndIf
1320     EndIf
1321 '
1322     If M_22# = MPass% Then M_20# = MPass%
1323     M_22# = MClear%
1324     If MRtn = 1 And M_20# = MClear% Then GoTo *CompRead
1325 '    fErrorProcess(11,17,0,0)
1326 '    Dly 10                                      'デバッグ用
1327 '    If M_In(11359) = 1 Then M_22# = MIrregular%'デバッグ用
1328 '    If M_22# = MIrregular% Then *ASSY_ERROR_END  'デバッグ用
1329 '    If M_20# = MPass% Then M_20# = MClear%      'デバッグ用
1330     If M_20# = MPass% Then
1331         M_22# = MIrregular%
1332         M_20# = MAssyOK%
1333         Dly 0.1
1334         Mvs PTicketRead_1                         '22/04/07 追加 渡辺
1335     EndIf
1336     If M_20# = MAssyOK% Then GoTo *AssyEnd
1337     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1338     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1339     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1340     If M_20# = MContinue% Then GoTo *RE_PIAS_CHECK
1341 *CompRead
1342 '
1343 'Mov PTicketRead_1
1344 Accel 25 , 25                'デバッグ中(3/28中村)
1345 Mov PMechaOnPltSet_2         'パレット回避点
1346 Accel 100 , 100              'デバッグ中(3/28中村)
1347 Mov PMechaOnPltSet_1         'パレット上空
1348 '
1349 '置く前にDVDを持っているか？確認    2022/04/12 渡辺
1350 'DVDを持っていおらず、チャック閉の場合、閉端の信号はONしない
1351 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
1352 If MRtn = 1 Then GoTo *DVDCheckEnd         'チャックの閉端がONの場合
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
1363 Mvs PMechaOnPltSet           'DVDメカ置き場所
1364 Dly 0.1
1365 '
1366 *RE_PLT_SET
1367 '
1368 M_Out(12256) = 0             'DVDメカチャック閉OFF
1369 M_Out(12257) = 1             'DVDメカチャック開ON
1370 '
1371 'Wait M_In(11265) = 1         'DVDメカチャック開検出
1372 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
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
1383 Mvs PMechaOnPltSet_1         'パレット上空
1384 'Ovrd 100
1385     MRtn = FnCtlValue2(2)          '組立ＯＫ＋１  2022/04/28 渡辺
1386 Mov PMechaOnJigGet_3
1387     MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1388 'Mov PInitialPosition   'コメントアウト(1/20中村)
1389 '
1390 'Wait M_In(11876) = 1         'ねじロボ3ねじ締め完了を受信
1391 'MRtn = frInCheck(11876,1,MSETTIMEOUT05&)   'ねじロボ3ねじ締め完了を受信・
1392 'If MRtn = 0 Then
1393 '    fErrorProcess()         'エラー処理
1394 'EndIf
1395 'M_Out(12868) = 1 Dly 0.5     'ねじロボ3ねじ締め完了を送信(処理位置変更3/26中村)
1396 'チケットID書き込み
1397 'If M_20# <> MPass% Then M_20# = MAssyOK%
1398 M_20# = MAssyOK%
1399 '
1400 *ASSY_ERROR_END
1401 *AssyEnd
1402 *fnAssyStart_FEndPosi
1403 FEnd
1404 '
1405 '■fnPiasCheck
1406 ''' <summary>
1407 ''' PIASチケット読込み
1408 ''' </summary>
1409 ''' <returns>   0 : NG
1410 '''             1 : OK(読込み完了)
1411 ''' </returns>
1412 ''' <remarks>
1413 ''' Date   : 2021/07/07 : M.Hayakawa
1414 ''' </remarks>'
1415 Function M% fnPiasCheck
1416     fnPiasCheck = 0
1417     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1418     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1419 '
1420 *RETRY_PIAS
1421     M_20# = MClear%
1422     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1423     '
1424     '【IDチケット読み込み】
1425     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1426     MInspGroup%(1) = 1              '検査G番号
1427     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1428 '
1429     'エラーの場合
1430     If MRtn <> 1 Then
1431         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1432         If MRtn <> 1 Then
1433             'D720 -> D1300 コピー要求
1434             M_Out(12565) = 1
1435             Dly 0.5
1436             M_Out(12565) = 0
1437             'エラー処理記述
1438             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1439             'GOT KEY入力待ち
1440             MKeyNumber = fnKEY_WAIT()
1441         '
1442             Select MKeyNumber
1443                 Case MNext%         '次へを選択した場合
1444                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1445                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1446 '                    GoTo *fnPiasCheck_End                   'PIASチェック終了(関数外部で分岐するよう変更3/26中村)
1447                     Break
1448                 Case MAbout%        '停止を選択した場合
1449                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1450                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1451 '                    GoTo *fnPiasCheck_End                   'PIASチェック終了(関数外部で分岐するよう変更3/26中村)
1452                     Break
1453                 Case MNgProcess%    'NGを選択した場合
1454                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1455                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1456 '                    GoTo *fnPiasCheck_End                   'PIASチェック終了(関数外部で分岐するよう変更3/26中村)
1457                     Break
1458                 Case MContinue%     '継続を選択した場合
1459                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1460                     M_20# = MContinue%
1461 '                    GoTo *RETRY_PIAS                        'PIASチェックリトライ(関数外部で分岐するよう変更3/26中村)
1462                     Break
1463             End Select
1464         EndIf
1465     EndIf
1466     If M_20# <> MClear% Then GoTo *fnPiasCheck_End
1467 '
1468 '----------D720 -> D1300 コピー要求----------
1469     M_Out(12565) = 1
1470     Dly 0.5
1471     M_Out(12565) = 0
1472 '----------通信確認をする----------
1473     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1474     MRtn = 0                ' 初期化
1475     M_20# = MClear%         ' 初期化
1476     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1477     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）(関数外部で分岐するように変更3/26中村)
1478     If MRtn <> 1 Then GoTo *fnPiasCheck_End
1479 '        If M_20# = MContinue% Then
1480 '            GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1481 '        Else
1482 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1483 '        EndIf
1484 '    EndIf
1485 '----------工程抜け確認----------
1486     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1487     MRtn = 0                ' 初期化
1488     M_20# = MClear%         ' 初期化
1489     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1490     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）(関数外部で分岐するように変更3/26中村)
1491     If MRtn <> 1 Then GoTo *fnPiasCheck_End
1492 '        If M_20# = MContinue% Then
1493 '            GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1494 '        Else
1495 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1496 '        EndIf
1497 '    EndIf
1498     '
1499     fnPiasCheck = 1
1500     *fnPiasCheck_End
1501 FEnd
1502 '
1503 '■fnPCComuCheck
1504 ''' <summary>
1505 ''' PC-PLC通信チェック
1506 ''' </summary>
1507 ''' <returns>   0 : NG
1508 '''             1 : OK(読込み完了)
1509 ''' </returns>
1510 ''' <remarks>
1511 ''' Date   : 2021/07/07 : M.Hayakawa
1512 ''' </remarks>'
1513 Function M% fnPCComuCheck
1514     fnPCComuCheck = 0
1515     MJudge% = 0                                  '初期化
1516     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1517     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1518     '
1519     For MStaNo = 0 To 5
1520         '
1521         If M_In(MIN_PIAS_ComOK%) = 1 Then
1522             'PC通信OK(M400)
1523             MJudge% = MOK%
1524             MStaNo = 5
1525             Break
1526         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1527             'toRBT_通信確認time out
1528             MJudge% = MNG%
1529             MCommentD1001 = 15
1530             MCommentD1002 = 21
1531             MStaNo = 5
1532             Break
1533         Else
1534             'toRBT_通信確認time out
1535             MJudge% = MNG%
1536             MCommentD1001 = 14
1537             MCommentD1002 = 21
1538             Break
1539         EndIf
1540     Next MStaNo
1541     '
1542     '上記で返信フラグを受信してからPC通信確認OFF
1543     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1544     '
1545     'エラー画面
1546     If MJudge% <> MOK% Then
1547         M_20# = MClear%     '初期化
1548         'エラー処理記述
1549         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1550         'GOT KEY入力待ち
1551         MKeyNumber = fnKEY_WAIT()
1552         '
1553         If MKeyNumber = MAbout% Then            '停止を選択した場合
1554             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1555             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1556             Break
1557         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1558             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1559             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1560             Break
1561         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1562             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1563             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1564             Break
1565         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1566             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1567             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1568             Break
1569         EndIf
1570     Else
1571         'OKの場合
1572         fnPCComuCheck = 1
1573     EndIf
1574 FEnd
1575 '
1576 '■fnProcessCheck
1577 ''' <summary>
1578 ''' 工程抜け確認
1579 ''' </summary>
1580 ''' <returns>    1：工程履歴OK     0：異常終了
1581 '''             -1：前工程履歴NG  -2：自工程履歴あり
1582 '''             -3：モデル仕向NG  -4：タイムアウト
1583 '''             -5：履歴処理エラー
1584 ''' </returns>
1585 ''' <remarks>
1586 ''' Date   : 2021/07/07 : M.Hayakawa
1587 ''' </remarks>'
1588 Function M% fnProcessCheck
1589     fnProcessCheck = 0
1590     MJudge% = MNG%      '一旦NGを初期化とする
1591 '----------工程抜け確認----------
1592     MCommentD1001 = 0   'コメント初期化
1593     For MStaNo = 0 To 5
1594         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1595         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1596         '
1597         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1598             MJudge% = MOK%
1599             fnAutoScreenComment(85)     ' AUTO画面
1600             MStaNo = 5
1601             Break
1602         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1603             MFlgLoop% = 0
1604             MJudge% = MNG%
1605             MCommentD1001 = 27
1606             MCommentD1002 = 22
1607             fnAutoScreenComment(94)     ' AUTO画面
1608             fnProcessCheck = -2         ' NGは-2を返す
1609             MStaNo = 5
1610             Break
1611         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1612            MJudge% = MNG%
1613             MCommentD1001 = 31
1614             MCommentD1002 = 22
1615             fnAutoScreenComment(83)     ' AUTO画面
1616             fnProcessCheck = -3         ' NGは-3を返す
1617             MStaNo = 5
1618             Break
1619         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1620             '履歴NGは直ぐに終了せず繰り返し確認を行う
1621             '前工程の書込みが終了していない可能性があるため
1622             MJudge% = MNG%
1623             MCommentD1001 = 32
1624             MCommentD1002 = 22
1625             fnAutoScreenComment(84)     ' AUTO画面
1626             fnProcessCheck = -1         ' NGは-1を返す
1627             Dly 1.0
1628             '工程抜け確認OFF
1629             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1630             Dly 1.0
1631            'MStaNo = 5
1632             Break
1633         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1634             MFlgLoop% = 0
1635             MJudge% = MNG%
1636             MCommentD1001 = 29
1637             MCommentD1002 = 22
1638             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1639             fnProcessCheck = -5         ' NGは-5を返す
1640             MStaNo = 5
1641             Break
1642         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1643             MJudge% = MNG%
1644             If MCommentD1001 = 32 Then
1645                 '何もしない
1646             Else
1647                 MCommentD1001 = 26
1648             EndIf
1649             MCommentD1002 = 22
1650             fnProcessCheck = -4         ' NGは-4を返す
1651             MStaNo = 5
1652             Break
1653         Else
1654             MJudge% = MNG%
1655             MCommentD1001 = 28
1656             MCommentD1002 = 22
1657         EndIf
1658     Next MStaNo
1659     '工程抜け確認OFF
1660     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1661     '通過履歴NG 工程抜けの場合
1662     If MJudge% = MPass% Then
1663         M_20# = MPass%
1664     EndIf
1665     '
1666     'エラー画面
1667     If MJudge% <> MOK% Then
1668         M_20# = MClear%     '初期化
1669         'エラー処理記述
1670         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1671         'GOT KEY入力待ち
1672         MKeyNumber = fnKEY_WAIT()
1673         '
1674         Select MKeyNumber
1675             Case MAbout%        '停止を選択した場合
1676                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1677                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1678                 Break
1679             Case MNext%         '次へを選択した場合
1680                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1681                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1682                 Break
1683             Case MContinue%     '継続を選択した場合
1684                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1685                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1686                 Break
1687             Case MNgProcess%    'NGを選択した場合
1688                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1689                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1690                 Break
1691         End Select
1692     Else
1693         fnProcessCheck = 1  ' OKは1を返す
1694     EndIf
1695 FEnd
1696 '
1697 '■fnPiasWrite
1698 ''' <summary>
1699 ''' Pias 組立結果書込み要求
1700 ''' </summary>
1701 '''<param name="MFlg%">
1702 '''                 MOK%(1) = 工程履歴にOKを書込む
1703 '''                 MNG%(0) = 工程履歴にNGを書込む
1704 '''</param>
1705 '''<returns></returns>
1706 ''' <remarks>
1707 ''' Date   : 2021/07/07 : M.Hayakawa
1708 ''' </remarks>'
1709 Function M% fnPiasWrite(ByVal MFlg%)
1710       fnPiasWrite = 0
1711 *RETRY_PIASWRITE
1712     '
1713     '組立OK(MOK%)の場合　M306 ON
1714    '組立NG(MNG%)の場合　M307 ON
1715     If MFlg% = MOK% Then
1716         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1717     Else
1718         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1719     EndIf
1720     Dly 0.1                  '念のため
1721     '
1722     'Piasへ書込み開始 M305 -> ON
1723     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1724     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1725     '
1726     MJudge% = MNG%
1727     '
1728     For MStaNo = 0 To 5
1729         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1730             MJudge% = MOK%
1731             'MRet = fnAutoScreenComment(85)  'AUTO画面
1732             MStaNo = 5
1733             Break
1734         '
1735         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1736             MJudge% = MNG%
1737             'MRet = fnAutoScreenComment(85)  'AUTO画面
1738            MCommentD1001 = 34
1739            MCommentD1002 = 25
1740             MStaNo = 5
1741             Break
1742         '
1743         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1744             MJudge% = MNG%
1745             'MRet = fnAutoScreenComment(85)  'AUTO画面
1746            MCommentD1001 = 35
1747            MCommentD1002 = 25
1748             MStaNo = 5
1749             Break
1750         '
1751         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1752             MJudge% = MNG%
1753             'MRet = fnAutoScreenComment(85)  'AUTO画面
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
1768     'Piasへ書込み開始 M305 -> OfF
1769     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1770     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1771     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1772     '
1773     '
1774     '通過履歴NG 工程抜けの場合
1775     If MJudge% = MPass% Then
1776         M_20# = MPass%
1777     EndIf
1778     '
1779    M_20# = MClear%     '初期化
1780     '
1781     'エラー画面
1782     If MJudge% < MOK% Then
1783     '
1784 '残しておくが現状では使用しないラベル
1785 *RETRY_ERR_WRITE
1786         M_20# = MClear%     '初期化
1787         'エラー処理記述
1788         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1789         'GOT KEY入力待ち
1790         MKeyNumber = fnKEY_WAIT()
1791         '
1792         If MKeyNumber = MAbout% Then   '停止を選択した場合
1793             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1794            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1795             Break
1796         '
1797         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1798             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1799             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1800         '
1801         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1802             M_20# = MPass%            'M_20# プログラム間共通外部変数
1803             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1804         '
1805         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1806             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1807            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
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
1822 '■fnPCBNumberCheck
1823 ''' <summary>
1824 ''' Pias 基板番号照合要求
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
1836     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1837     'Piasへ基板照合開始 M310 -> ON
1838     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1839     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1840     '
1841     MJudge% = MNG%
1842     '
1843     For MStaNo = 0 To 5
1844         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1845             MJudge% = MOK%
1846             fnAutoScreenComment(96)  'AUTO画面
1847             MStaNo = 5
1848             Break
1849         '
1850         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1851             MJudge% = MNG%
1852             fnAutoScreenComment(97)  'AUTO画面
1853             MCommentD1001 = 37
1854             MCommentD1002 = 25
1855             MStaNo = 5
1856             Break
1857         '
1858         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1859             MJudge% = MNG%
1860             fnAutoScreenComment(98)  'AUTO画面
1861             MCommentD1001 = 38
1862             MCommentD1002 = 25
1863             MStaNo = 5
1864             Break
1865         '
1866         ElseIf M_In(11580) = 1 Then                         'time out
1867             MJudge% = MNG%
1868             fnAutoScreenComment(99)  'AUTO画面
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
1883     'Piasへ基板照合開始 M310 -> OfF
1884     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1885     '
1886     '
1887     '通過履歴NG 工程抜けの場合
1888     If MJudge% = MPass% Then
1889         M_20# = MPass%
1890     EndIf
1891     '
1892    M_20# = MClear%     '初期化
1893     '
1894     'エラー画面
1895     If MJudge% < MOK% Then
1896     '
1897 '残しておくが現状では使用しないラベル
1898 *RETRY_ERR_PCBNUMBER
1899         M_20# = MClear%     '初期化
1900         'エラー処理記述
1901         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1902         'GOT KEY入力待ち
1903         MKeyNumber = fnKEY_WAIT()
1904         '
1905         If MKeyNumber = MAbout% Then   '停止を選択した場合
1906             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1907             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1908             Break
1909         '
1910         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1911             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1912             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1913         '
1914         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1915             M_20# = MPass%            'M_20# プログラム間共通外部変数
1916             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1917         '
1918         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1919             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1920             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
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
1932 '■ScrewTight_S2
1933 ''' <summary>
1934 ''' ねじ締めを行う
1935 ''' </summary>
1936 '''<param name="PScrewPos()">
1937 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1938 '''             PScrewPos(2)    ：ねじ締め回避点
1939 '''             PScrewPos(10)   ：ねじ締め終了高さ
1940 '''</param>
1941 '''<returns>整数
1942 '''         0=異常終了、1=正常終了
1943 '''</returns>
1944 ''' <remarks>
1945 ''' Date   : 2021/07/07 : M.Hayakawa
1946 ''' </remarks>'
1947 Function M% ScrewTight_S2(ByVal PScrewPosition())   'ネジ締め個別設定
1948     ScrewTight_S2 = 0
1949     MOKNGFlg = 0
1950     Ovrd 100
1951     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1952     ' 暫定
1953     Ovrd 5
1954     Mvs PScrewPosition(10),-10    ' パレット上ねじ締めS①の上空へ移動
1955 '    Ovrd MOvrdA
1956     '暫定マスク
1957 '    M_Out(Y62_Driver)=1     ' バンクセッティング　C1
1958 '    Dly 0.1
1959 '    M_Out(Y61_Driver)=1     'ドライバーON　CW
1960 '    'Spd 8.3 '外部ライド100  内部ライド60   'ライド100-40　100%：Spd　15　'ねじ締め速度設定
1961 '    Spd MSpdA               'ネジ締め時Spd個別設定
1962     ' 暫定移動のみ
1963     Mvs PScrewPosition(10)
1964 '    '
1965 '    Dly 0.1
1966 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1967 '    Wait M_In(11584)=1          '完了/エラー検出
1968 '    Dly 0.1
1969 '    Spd M_NSpd
1970 '    '
1971 '    If M_In(X28_Driver)=1 Then  'ねじトータルエラー検出時
1972 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1973 '        Dly 0.1
1974 '        M_Out(Y62_Driver)=0     'バンクセッティング解除　C1
1975 '        Dly 0.1
1976 '        M_Out(Y63_Driver)=0     'バンクセッティング解除　C1
1977 '        Dly 0.1
1978 '        M_Out(Y65_Driver)=0     'プログラム解除　F1
1979 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1980 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
1981 '        MOKNGFlg = -1
1982 '        ScrewTight_S2 = 0
1983 '    Else
1984 '        Wait M_In(X29_Driver)=1 ' 正常完了時
1985 '        Dly 0.1
1986 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1987 '        Dly 0.1
1988 '        M_Out(Y62_Driver)=0     'バンクセッティング解除
1989 '        Dly 0.1
1990 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
1991 '        Dly 0.1
1992 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1993 '        ScrewTight_S2 = 1
1994 '    EndIf
1995 ' 暫定
1996     Ovrd 10
1997     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1998     Ovrd 100
1999 FEnd
2000 '
2001 '■ScrewGet_S3
2002 ''' <summary>
2003 ''' ねじ供給機からねじを得る
2004 ''' </summary>
2005 '''<param name="%"></param>
2006 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
2007 '''         PScrewPos(2)    ：ねじ供給器回避点
2008 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2009 '''         PScrewPos(3)    ：Mねじポカヨケ位置
2010 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
2011 '''<returns>整数
2012 '''         0=異常終了、1=正常終了、-1=MネジセンサーNG、-2=MネジセンサーON、-3=吸着エラー
2013 '''</returns>
2014 ''' <remarks>
2015 ''' Date   : 2021/07/07 : M.Hayakawa
2016 ''' </remarks>'
2017 Function M% ScrewGet_S3(ByVal PScrewPosition())
2018     ScrewGet_S3 = 0
2019     MMScrewJudge% = 0
2020     'ねじ供給器初期動作エラーチェック
2021 ' ↓暫定削除
2022 '    Wait M_In(X34_ScrewReady1)=1 'ねじ供給器SがReadyになるまで待つ　←　何秒か待ってReadyにならなければ抜けるプログラムが必要？
2023 '    Ovrd 100
2024 '    If M_In(X33_SS2)=0 Then  'Mねじ検出センサがOFF（故障）していた場合
2025 '        Ovrd 30
2026 '        Mvs,-80             'その場所から80mm上空へ移動
2027 '        Mov PInitPos19049   '19049初期位置へ移動
2028 '        M_Out(Y6A_VV1)=0    'ねじ吸着 Off
2029 '        'NGとしてここの関数から抜ける
2030 '        ScrewGet_S3 = -1
2031 '        MMScrewJudge% = 1
2032 '        MCommentD1001 = 61
2033 '    EndIf
2034 '    If ScrewGet_S3 = 0 Then
2035 '        'Sタイト用ねじ供給機にMねじが混入していないか監視
2036 '        MMScrewJudge% = 0 'MMScrewJudgeを初期化する
2037 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT05&)
2038 '        If MRtn = 0 Then
2039 '            Ovrd 30
2040 '            Mvs,-80            'その場所から50mm上空へ移動
2041 '            Mov PInitPos19049  '19049初期位置へ移動
2042 '            MMScrewJudge% = 2
2043 '            MRtn = All_CLamp_Release()'全てのクランプ解除へ分岐
2044 '            MCnt% = 2   '2を設定
2045 '            MCommentD1001 = 62
2046 '        EndIf
2047 '        If MMScrewJudge% = 2 Then
2048 '            ScrewGet_S3 = -2
2049 '        EndIf
2050 '    EndIf
2051 '    'Mネジ判定がONの場合 NGとして関数を抜ける
2052 '    If MMScrewJudge% = 2 Then
2053 '        ScrewGet_S3 = -2
2054 '    EndIf
2055     'Sネジ用ねじ太郎のMネジ混入確認用ここまで
2056     Ovrd 100
2057     Spd M_NSpd
2058     If MMScrewJudge% = 0 Then
2059         ScrewGet_S3 = 0
2060         M_Out(Y63_Driver)=1         ' バンクセッティング　C2
2061         MScrewCnt% = 0
2062         MFinCnt% = 2
2063 '        For MCnt% = 0 To MFinCnt%
2064             Mov PScrewPosition(2)        ' ねじ供給機回避点
2065             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
2066             Ovrd 80
2067             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2068             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
2069             Mvs PScrewPosition(10), 1.2
2070             M_Out(Y6A_VV1)=1        ' ねじ吸着　ON
2071             'ビット回転
2072             M_Out(Y60_Driver)=1
2073             Dly 0.2
2074             '
2075             Ovrd 100
2076             JOvrd M_NJovrd
2077             Spd M_NSpd
2078             'ネジ吸着確認位置移動
2079             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
2080             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
2081             'ビット回転停止
2082             'M_Out(Y60_Driver)=0
2083             '
2084             '1秒間ネジ吸着確認
2085 ' 以下暫定削除
2086 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2087 '            'MRtn = 0'強制エラー
2088 '            '吸着エラーの場合
2089 '            'ネジをねじ太郎に戻す
2090 '            If MRtn = 0 Then
2091 '                Ovrd 30
2092 '                'ビット回転停止
2093 '                M_Out(Y60_Driver)=0
2094 '                'ネジ供給機上空
2095 '                Mvs PScrewPos(1)
2096 '                '更に上空
2097 '                Mov PScrewPos(1), -75
2098 '                'ネジ捨て位置
2099 '                Mov PScrewFeedS021
2100 '                '吸着OFF
2101 '                M_Out(Y6A_VV1)=0 'ねじ吸着　OFF
2102 '                Dly 0.2
2103 '                '破壊ON
2104 '                M_Out(Y6B_VB1)=1 '真空破壊ON
2105 '                'ビット回転
2106 '                M_Out(Y61_Driver)=1
2107 '                Dly 0.5
2108 '                '
2109 '                Ovrd 100
2110 '                JOvrd M_NJovrd
2111 '                Spd M_NSpd
2112 '                'ドライバーを上下させねじを振り落とす
2113 '                Mov PScrewFeedS021, 10
2114 '                Mov PScrewFeedS021
2115 '                Dly 0.1
2116 '                Mov PScrewFeedS021, 10
2117 '                Mov PScrewFeedS021
2118 '                '
2119 '                'ネジ落ち待ち
2120 '                'ビット回転停止
2121 '                M_Out(Y61_Driver)=0
2122 '                Dly 0.1
2123 '                '破壊OFF
2124 '                M_Out(Y6B_VB1)=0 '真空破壊OFF
2125 '                '
2126 '                '
2127 '                'ねじ落ちたとして、移動更に上空
2128 '                Mov PScrewPos(1), -75
2129 '                Ovrd 100
2130 '                Spd M_NSpd
2131 '                'ネジ供給機上空
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
2145         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2146         M_Out(Y60_Driver)=0     ' ビット回転停止
2147         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
2148         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2149         'もう一度吸着確認
2150 ' 以下暫定削除
2151 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2152 '        If MRtn = 0 Then      '吸着エラーの場合
2153 '            MCommentD1001 = 94
2154 '            MCommentD1002 = 95
2155 '            ScrewGet_S3 = -3
2156 '        EndIf
2157 '        If MRtn = 1 Then      '吸着OKの場合
2158 '            ScrewGet_S3 = 1
2159 '        EndIf
2160 '        Break
2161     Else
2162         'Mネジ
2163         If MMScrewJudge% = 2 Then
2164             ScrewGet_S3 = -2
2165         EndIf
2166     EndIf
2167 FEnd
2168 '
2169 '■fnKEY_WAIT()
2170 ''' <summary>
2171 ''' GOTからのキー入力待ち
2172 ''' </summary>
2173 '''<returns>1：停止    2：次へ
2174 '''         3：継続    4：トルクチェック開始
2175 '''         5：NG
2176 '''         11：ロボット初期位置1    12：ロボット初期位置2
2177 '''         13：ロボット初期位置3    14：ロボット初期位置4
2178 '''</returns>
2179 ''' <remarks>
2180 ''' Date   : 2021/07/07 : M.Hayakawa
2181 ''' </remarks>'
2182 Function M% fnKEY_WAIT()
2183     fnKEY_WAIT = 0
2184     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
2185     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
2186     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
2187     '下記キー待ちの継続に反応させないため
2188     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
2189     Dly 0.2
2190     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
2191     MLocalLoopFlg=1
2192     While MLocalLoopFlg=1
2193         If M_In(11345) = 1 Then         '停止   M5345
2194             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
2195             fnKEY_WAIT = 1
2196             MLocalLoopFlg=-1
2197             Break
2198         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
2199             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
2200             fnKEY_WAIT = 2
2201             MLocalLoopFlg=-1
2202             Break
2203         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
2204             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
2205             fnKEY_WAIT = 3
2206             MLocalLoopFlg=-1
2207             Break
2208         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
2209             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
2210             fnKEY_WAIT = 4
2211             MLocalLoopFlg=-1
2212             Break
2213         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
2214             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
2215             fnKEY_WAIT = 5
2216             MLocalLoopFlg=-1
2217             Break
2218             '
2219         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
2220             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
2221             fnKEY_WAIT = MRobotInit1%
2222             MLocalLoopFlg=-1
2223             Break
2224             '
2225         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
2226             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
2227             fnKEY_WAIT = MRobotInit2%
2228             MLocalLoopFlg=-1
2229             Break
2230             '
2231         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
2232             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
2233             fnKEY_WAIT = MRobotInit3%
2234             MLocalLoopFlg=-1
2235             Break
2236             '
2237         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2238             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2239             fnKEY_WAIT = MRobotInit4%
2240             MLocalLoopFlg=-1
2241             Break
2242             '
2243         Else
2244         EndIf
2245     WEnd
2246     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2247     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2248 FEnd
2249 '
2250 '■ fnAUTO_CTL
2251 ''' <summary>
2252 ''' AUTOモードOFF、PLCからの開始待ち
2253 ''' </summary>
2254 ''' <remarks>
2255 ''' Date   : 2021/07/07 : M.Hayakawa
2256 ''' </remarks>
2257 Function M% fnAUTO_CTL
2258     fnAUTO_CTL = 0
2259     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2260     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2261     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2262     '
2263     If M_Svo=0 Then             'サーボON確認
2264         Servo On
2265     EndIf
2266     Wait M_Svo=1
2267 FEnd
2268 '
2269 '■ fnWindScreenOpen
2270 ''' <summary>
2271 ''' ウィンド画面の表示、非表示設定
2272 ''' </summary>
2273 '''<param name="%"></param>
2274 '''<param name="%"></param>
2275 '''<param name="%"></param>
2276 '''<param name="%"></param>
2277 ''' <remarks>
2278 ''' コメントD1001, D1002, D1003の設定
2279 ''' MWindReSet = 0     画面非表示
2280 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2281 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2282 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2283 ''' Date   : 2021/07/07 : M.Hayakawa
2284 ''' </remarks>
2285 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2286     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2287         M_Out16(12480) = MCommentD1001            'D1001 コメント
2288     EndIf
2289     '
2290     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2291         M_Out16(12496) = MCommentD1002            'D1002 コメント
2292     EndIf
2293     '
2294     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2295        M_Out16(12512) = MCommentD1003            'D1003 コメント
2296     EndIf
2297     '
2298     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2299     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2300     Dly 0.5
2301     M_Out(12363) = 0                         'ウィンド画面設定
2302 FEnd
2303 '
2304 '■FnCtlValue2
2305 ''' <summary>
2306 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2307 ''' </summary>
2308 ''' <param name="MCtlNo%"></param>
2309 ''' <remarks>
2310 ''' Date : 2022/04/28 渡辺
2311 ''' </remarks>
2312 '''
2313 '''  1：投入数       ＋１
2314 '''  2：組立ＯＫ数   ＋１
2315 '''  3：組立ＮＧ数   ＋１ (未使用)
2316 '''  4：吸着エラー数 ＋１
2317 ''' 99：読書開始信号 OFF
2318 '''
2319 Function M% FnCtlValue2(ByVal MCtlNo%)
2320     FnCtlValue2 = 1
2321     Select MCtlNo%
2322         Case 1        '投入数＋１
2323             M_Out(12569) = 0             '書込み開始信号OFF
2324             M_Out(12568) = 1             '読込み開始信号ON
2325             MInputQty = M_In16(11600)    '投入数受信
2326             MInputQty = MInputQty + 1    '投入数＋１
2327             M_Out16(12592) = MInputQty   '投入数送信
2328             M_Out(12569) = 1             '書込み開始信号ON
2329             Break
2330             '
2331         Case 2        '組立ＯＫ数＋１
2332             M_Out(12569) = 0             '書込み開始信号OFF
2333             M_Out(12568) = 1             '読込み開始信号ON
2334             MAssyOkQty = M_In16(11616)   '組立OK数受信
2335             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2336             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2337             M_Out(12569) = 1             '書込み開始信号ON
2338             Break
2339             '
2340         Case 4        '吸着エラー数＋１
2341             M_Out(12569) = 0                       '書込み開始信号OFF
2342             M_Out(12568) = 1                       '読込み開始信号ON
2343             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2344             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2345             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2346             M_Out(12569) = 1                       '書込み開始信号ON
2347             Break
2348             '
2349         Case 99        '読書開始信号OFF
2350             M_Out(12568) = 0        '読込み開始信号OFF
2351             M_Out(12569) = 0        '書込み開始信号OFF
2352             Break
2353             '
2354     End Select
2355     Exit Function
2356 FEnd
2357 '
2358 'Insightによる画像処理検査実行（並列処理なし）
2359 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2360 '-------------------------------------------------------------------------------
2361 'Insightによる画像処理検査実行（並列処理なし）
2362 '   引数
2363 '       PInspPos()      ：検査位置
2364 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2365 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2366 '       MInspCnt%       ：検査位置数
2367 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2368 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2369 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2370 '   戻り値：整数
2371 '       0=異常終了、1=正常終了
2372 '
2373 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2374 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2375 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2376 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2377 '   20200410    :   検査グループ設定Retry追加
2378 '-------------------------------------------------------------------------------
2379     '----- 初期設定 -----
2380     Cnt 0                                                           '移動効率化解除(初期値=0)
2381     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2382 '    Cnt 1,0.1,0.1
2383     '変数宣言・初期化
2384     Def Inte MNum                                                   '検査番号(検査順1～)
2385     MNum% = 1                                                       '検査番号初期値設定
2386     Def Inte MEndFlg                                                '検査終了フラグ
2387     MEndFlg% = 0
2388     '
2389     '検査G番号設定要求・検査実行要求off
2390     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2391     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2392     'エラー番号クリア
2393     MInspErrNum = 0                                                 '検査実行エラー番号
2394     M_Out16(MOUT_InspErrNum) = MInspErrNum
2395     MInspNGStepNum = 0                                              '検査実行NGStep番号
2396     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2397     '
2398     'Insight Ready check?
2399     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2400         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2401         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2402         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2403         ISInspectionSingle = 0                                      '異常終了戻り値設定
2404         Exit Function
2405     EndIf
2406     '
2407     '検査位置数確認
2408     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2409         MInspErrNum = 21                                            '検査データなし 21　引数<1
2410         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2411         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2412         ISInspectionSingle = 0                                      '異常終了戻り値設定
2413         Exit Function
2414     EndIf
2415     '
2416     '
2417     '
2418     '----- メイン処理 -----
2419     '設定された検査位置数分の検査実行
2420     While( MEndFlg% = 0 )
2421         '----- 検査グループ番号設定Retry追加 20200410
2422         MSetGrNumRetryExitFlg = 0
2423         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2424         While( MSetGrNumRetryExitFlg = 0 )
2425         '----- 検査グループ番号設定Retry追加ここまで 20200410
2426             '
2427             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2428             '
2429             '----- 検査グループ番号設定 -----
2430             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2431             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2432             '
2433             '検査位置へ移動・移動完了待ち
2434             Mvs PInspPos( MNum% )                                       '移動
2435             Dly 0.05                                                    '移動完了後Delay
2436             '
2437             '検査グループ番号設定終了確認
2438             M_Timer(1) = 0
2439             MExitFlg = 0
2440             While( MExitFlg = 0 )
2441                 '検査G設定正常終了?
2442                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2443                     MExitFlg = 1
2444                 '
2445                 '検査G設定異常終了?
2446                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2447                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2448                     If MInspErrNum = 0 Then                             '1回目のエラー?
2449                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2450                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2451                     EndIf
2452                     MExitFlg = 1
2453                 '
2454                 'timeoutチェック
2455                 ElseIf 1000 < M_Timer(1) Then
2456                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2457                     If MInspErrNum = 0 Then                             '1回目のエラー?
2458                         MInspErrNum = 12                                'timeout エラー番号=12
2459                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2460                     EndIf
2461                     MExitFlg = 1
2462                 EndIf
2463             WEnd
2464             '
2465             '検査G番号設定要求off
2466             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2467             '
2468             '----- 検査グループ設定Retry追加 20200410
2469             'NGなければ抜ける
2470             If MCurrentStepErr = 0 Then
2471                 MSetGrNumRetryExitFlg = 1
2472             Else
2473                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2474                 If MSetGrNumRetryCnt = 0 Then
2475                     MSetGrNumRetryExitFlg = 1
2476                 Else
2477                     'Retryへ　その前にDelay
2478                     Dly 0.5
2479                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2480                 EndIf
2481             EndIf
2482             '----- 検査グループ設定Retry追加ここまで 20200410
2483             '
2484         WEnd
2485         '
2486         '
2487         '
2488         '----- 検査実行 -----
2489         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2490             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2491                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2492                 MInspRetryExitFlg = 0
2493                 MRetryCnt = 2                                        'Retry回数設定
2494                 While( MInspRetryExitFlg = 0 )
2495                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2496                     '
2497                     '検査完了確認
2498                     MRetryCnt = MRetryCnt - 1
2499                     M_Timer(1) = 0
2500                     MExitFlg = 0
2501                     While( MExitFlg = 0 )
2502                     '検査完了待ち
2503                         '検査OK終了?
2504                         If M_In( MIN_IS_InspOK% ) = 1  Then
2505                             MJudgeOKFlg = 1                         '検査OKフラグON
2506                             MExitFlg = 1
2507                         '
2508                         '検査NG終了?
2509                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2510                             If MInspErrNum = 0 Then                 '1回目のエラー?
2511                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2512                                     MInspErrNum = 32                    '検査NG エラー番号=32
2513                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2514                                 EndIf
2515                             EndIf
2516                             MExitFlg = 1
2517                         '
2518                         '検査異常終了(IS timeout)?
2519                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2520                             If MInspErrNum = 0 Then                 '1回目のエラー?
2521                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2522                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2523                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2524                                 EndIf
2525                             EndIf
2526                             MExitFlg = 1
2527                         '
2528                         'timeoutチェック
2529                         ElseIf 3000 < M_Timer(1) Then
2530                             If MInspErrNum = 0 Then                 '1回目のエラー?
2531                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2532                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2533                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2534                                 EndIf
2535                             EndIf
2536                             MExitFlg = 1
2537                         EndIf
2538                     WEnd
2539                     '
2540                     '検査開始要求off
2541                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2542                     '
2543                     'OKなら抜ける
2544                     If MJudgeOKFlg = 1 Then
2545                         MInspRetryExitFlg = 1
2546                     Else
2547                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2548                         If MRetryCnt = 0 Then
2549                             MInspRetryExitFlg = 1
2550                         Else
2551                             'Retryへ　その前にDelay
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
2562         MNum% = MNum% + 1                                           '検査Step+1
2563         '検査終了確認　検査終了フラグセット
2564         If (MInspCnt% < MNum% ) Then
2565             MEndFlg% = 1                                            '検査終了フラグセット
2566         EndIf
2567         'NG発生時続行時処理
2568         If MInspErrNum <> 0 Then                                    'NGあり?
2569             If MNgContinue% <> 1 Then                               'NG続行?
2570                 MEndFlg% = 1                                        '検査終了フラグセット
2571             EndIf
2572         EndIf
2573     WEnd
2574     '
2575     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2576     If 0 < MZAxis% Then
2577         PCurrentPos = P_Curr                                        '現在位置取得
2578         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2579         Mvs PCurrentPos                                             '現在位置上空へ移動
2580     EndIf
2581     '
2582     '戻り値設定
2583     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
2584         ISInspectionSingle = 1                                      '正常終了戻り値設定
2585     Else
2586         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2587         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2588         ISInspectionSingle = 0                                      '異常終了戻り値設定
2589     EndIf
2590     Fine 0 , P
2591     '
2592 FEnd
2593 '
2594 ' ■ISInspection
2595 ''' <summary>
2596 ''' Insightによる画像処理検査実行
2597 ''' </summary>
2598 '''<param name="PInspPos()">検査位置</param>
2599 '''<param name="MInspGrNum%()">検査位置での検査グループ番号（=0：画像検査未実施）</param>
2600 '''             PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2601 '''<param name="MInspCnt%">検査位置数</param>
2602 '''<param name="MZAxis%">終了時のZ軸退避座標（-1:無効）</param>
2603 '''             終了時にZ軸をMZAxisで設定された位置まで上昇させる
2604 '''<param name="MNgContinue%">=1で検査エラー・NG発生時に全Stepの検査を行う</param>
2605 '''<returns>    整数 0=異常終了、1=正常終了</returns>
2606 '''         MInspErrNum     ：異常終了時にエラー番号が設定される
2607 '''         MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される"
2608 ''' <remarks>
2609 ''' Date   : 2021/07/07 : M.Hayakawa
2610 ''' </remarks>
2611 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2612 '    '画像使用確認 0<- 画像確認無しの場合
2613 '    If M_In(11369) = 0 Then            'toRBT_使用確認
2614 '        ISInspection = 1                                        '正常終了戻り値設定
2615 '    EndIf
2616 ''
2617 '    Cnt 0                                                       '移動効率化解除(初期値=0)
2618 '    Fine 0.05,P                                                 '位置決め完了条件設置　0.05mm
2619 '    MNum% = 1                                                   '検査番号初期値設定
2620 '    Def Inte MEndFlg                                            '検査終了フラグ
2621 '    MEndFlg% = 0
2622 '    '
2623 '    'エラー番号クリア
2624 '    MInspErrNumSub = 0                                          '検査実行エラー番号sub
2625 '    MInspErrNum = 0                                             '検査実行エラー番号
2626 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2627 '    MInspNGStepNum = 0                                          '検査実行NGStep番号
2628 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2629 '    '
2630 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready offなら終了
2631 '        MInspErrNum = 20                                        '検査実行エラー番号 20 Insight offline
2632 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2633 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2634 '        ISInspection = 0                                        '異常終了戻り値設定
2635 ''
2636 '    EndIf
2637 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2638 '    '
2639 '    '検査位置数確認
2640 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2641 '        MInspErrNum = 21                                        '検査データなし 21　引数<1
2642 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2643 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2644 '        ISInspection = 0                                        '異常終了戻り値設定
2645 ''
2646 '    EndIf
2647 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2648 '    '
2649 '    '設定された検査位置数分の検査実行
2650 '    While( MEndFlg% = 0 )
2651 '        '検査終了確認　検査終了フラグセット
2652 '        If (MInspCnt% < MNum% ) Then
2653 '            MEndFlg% = 1                                        '検査終了フラグセット
2654 '        EndIf
2655 '        '
2656 '        'タスク　検査G番号設定・検査完了確認処理開始　INSPTAST1
2657 '        If MEndFlg% = 0 Then
2658 '            M_01# = MInspGrNum%(MNum%)                          '検査G番号引渡し
2659 '        EndIf
2660 '        M_02# = MEndFlg%                                        '検査終了フラグ引渡し
2661 '        M_05# = MNum%                                           '検査番号(検査順1～)
2662 '        'タスク　検査G設定フラグ引渡し
2663 '        If MEndFlg% = 0 Then
2664 '            If 0 < MInspGrNum%(MNum%) Then
2665 '                M_03# = 1
2666 '            Else
2667 '                M_03# = 0
2668 '            EndIf
2669 '        Else
2670 '            M_03# = 0
2671 '        EndIf
2672 '        'タスク　検査結果確認フラグ引渡し
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
2683 '        'タスク処理開始
2684 '        M_00# = 1                                               'TASK処理開始
2685 '        'タスク処理開始確認
2686 '        M_Timer(1) = 0
2687 '        MExitFlg = 0
2688 '        While( MExitFlg = 0 )
2689 '            '処理開始完了確認
2690 '            If M_00# = 0 And M_10# = 8 Then
2691 '                MExitFlg = 1
2692 '            EndIf
2693 '            'timeoutチェック
2694 '            If 2000 < M_Timer(1) Then
2695 '                If MNgContinue% = 1 Then                        'NG続行?
2696 '                    MInspErrNumSub = 36                         'エラー番号設定36
2697 '                Else
2698 '                    MInspErrNum = 36                            'エラー番号設定36
2699 '                EndIf
2700 '                MExitFlg = 1
2701 '            EndIf
2702 '        WEnd
2703 '        '
2704 '        '検査位置へ移動・移動完了待ち
2705 '        If 0 = MInspErrNum Then
2706 '            If MEndFlg% = 0 Then
2707 '                Mvs PInspPos( MNum% )                           '移動
2708 '            EndIf
2709 '        EndIf
2710 '        '
2711 '        'タスク　検査G番号設定・検査完了確認処理終了待ち　INSPTAST1
2712 '        If 0 = MInspErrNum Then
2713 '            M_Timer(1) = 0
2714 '            MExitFlg = 0
2715 '            While( MExitFlg = 0 )
2716 '                '処理完了待ち（正常終了）
2717 '                If M_10# = 1 Then
2718 '                    MExitFlg = 1
2719 '                EndIf
2720 '                '処理完了待ち（異常終了）
2721 '                If M_10# = 0 Then
2722 '                    If MNgContinue% = 1 Then                    'NG続行?
2723 '                        MInspErrNumSub = M_12#                  'エラー番号設定　M12
2724 '                    Else
2725 '                        MInspErrNum = M_12#                     'エラー番号設定　M12
2726 '                    EndIf
2727 '                    MExitFlg = 1
2728 '                EndIf
2729 '                'timeoutチェック
2730 '                If 5000 < M_Timer(1) Then
2731 '                    If MNgContinue% = 1 Then                    'NG続行?
2732 '                        MInspErrNumSub = 31                     'エラー番号設定31
2733 '                    Else
2734 '                        MInspErrNum = 31                        'エラー番号設定31
2735 '                    EndIf
2736 '                    MExitFlg = 1
2737 '                EndIf
2738 '            WEnd
2739 '        EndIf
2740 '        '
2741 '        '検査結果確認
2742 '        If 0 = MInspErrNum Then
2743 '            If 1 < MNum% Then
2744 '                If 0 < MInspGrNum%(MNum%-1) Then                '検査あり?
2745 '                    If M_11# = 2 Then                           '検査NG?
2746 '                        If MNgContinue% = 1 Then                'NG続行?
2747 '                            If MInspNGStepNum = 0 Then          'NG未発生?
2748 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2749 '                            EndIf
2750 '                            MInspErrNumSub = 32                 'エラー番号設定 32:検査NG
2751 '                        Else
2752 ''                            MInspNGStepNum = MNum% - 1          '検査実行NGStep番号設定
2753 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2754 '                            MInspErrNum = 32                    'エラー番号設定 32:検査NG
2755 '                        EndIf
2756 '                   EndIf
2757 '                EndIf
2758 '            EndIf
2759 '        EndIf
2760 '        '
2761 '        'エラーなら検査中断終了するのでLoopから抜けるため終了フラグセット
2762 '        If 0 <> MInspErrNum Then
2763 '            MEndFlg% = 1
2764 '        EndIf
2765 '        '
2766 '        '検査実行、取込完了待ち
2767 '        If 0 = MInspErrNum Then
2768 '            If MEndFlg% = 0 Then
2769 '                If 0 < MInspGrNum%(MNum%) Then                  '検査あり?
2770 '                    M_Out(MOUT_IS_Insp%) = 1                    '検査実行要求on
2771 '                    '取込完了確認
2772 '                    M_Timer(1) = 0
2773 '                    MExitFlg = 0
2774 '                    While( MExitFlg = 0 )
2775 '                        '処理完了待ち
2776 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2777 '                            MExitFlg = 1
2778 '                        EndIf
2779 '                        'timeoutチェック
2780 '                        If 2000 < M_Timer(1) Then
2781 '                            If MNgContinue% = 1 Then            'NG続行?
2782 '                                MInspErrNumSub = 33             'エラー番号設定33
2783 '                            Else
2784 '                                MInspErrNum = 33                'エラー番号設定33
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
2796 '    '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2797 '    If 0 < MZAxis% Then
2798 '        PCurrentPos = P_Curr                                    '現在位置取得
2799 '        PCurrentPos.Z = MZAxis%                                 'Z軸を設定
2800 '        Mvs PCurrentPos                                         '現在位置上空へ移動
2801 '    EndIf
2802 '    '
2803 '    'NG続行時処理
2804 '    If MNgContinue% = 1 Then                                    'NG続行?
2805 '        MInspErrNum = MInspErrNumSub                            'エラー番号設定
2806 '    EndIf
2807 '    '
2808 '    '戻り値設定
2809 '    If MInspErrNum = 0 Then
2810 '        ISInspection = 1                                        '正常終了戻り値設定
2811 '    Else
2812 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2813 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2814 '        ISInspection = 0                                        '異常終了戻り値設定
2815 '    EndIf
2816 '    '
2817 '*ISInspection_End
2818 'FEnd
2819 '
2820 '■InitialZoneB
2821 ''' <summary>
2822 ''' 非常停止後の復帰動作
2823 ''' 1)上空退避　Z方向上に移動
2824 ''' 2)J1軸以外を退避ポジションへ移動
2825 ''' 3)J1軸のみを退避ポジションへ移動
2826 ''' 4)イニシャルポジションへ移動
2827 ''' </summary>
2828 ''' <remarks>
2829 ''' Date : 2022/03/23 : N.Watanabe
2830 ''' </remarks>
2831 Function V fnInitialZoneB()
2832     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/04/26 渡辺
2833 '
2834 'パラメータ
2835     Ovrd 5
2836 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2837 '    Cmp Pos, &B100011
2838 '
2839 '復帰動作開始
2840 '
2841 '置き台と両掴みの場所は、チャックを解放する
2842 *RecoveryChuckOpen
2843     PActive = P_Curr          '現在位置を取得
2844     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
2845 'PMechaOnRoboSet(DVDメカ置き場)は、チャック解放
2846     If (PActive.X <= PMechaOnRoboSet.X + 1.0) And (PActive.X >= PMechaOnRoboSet.X -1.0) Then
2847         If (PActive.Y <= PMechaOnRoboSet.Y + 1.0) And (PActive.Y >= PMechaOnRoboSet.Y -1.0) Then
2848             If (PActive.Z <= PMechaOnRoboSet.Z + 1.0) And (PActive.Z >= PMechaOnRoboSet.Z -1.0) Then
2849                 MRecoveryChuckOpen = 1
2850             EndIf
2851         EndIf
2852     EndIf
2853 'PMechaOnRoboGet(DVDメカ受け取り位置)は、チャック解放
2854     If (PActive.X <= PMechaOnRoboGet.X + 1.0) And (PActive.X >= PMechaOnRoboGet.X -1.0) Then
2855         If (PActive.Y <= PMechaOnRoboGet.Y + 1.0) And (PActive.Y >= PMechaOnRoboGet.Y -1.0) Then
2856             If (PActive.Z <= PMechaOnRoboGet.Z + 1.0) And (PActive.Z >= PMechaOnRoboGet.Z -1.0) Then
2857                 MRecoveryChuckOpen = 1
2858             EndIf
2859         EndIf
2860     EndIf
2861 '
2862     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2863     M_Out(12256) = 0                           'DVDチャック閉OFF
2864     M_Out(12257) = 1                           'DVDチャック開ON
2865 '
2866     M_20# = 0                                  'KEY入力初期化
2867     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
2868     If MRtn = 1 Then M_Out(12257) = 0          'DVDチャック開OFF
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
2879 '特殊回避　直接、上空退避が出来ない所の対処
2880 '
2881 'PMechaOnRoboSet(Get)～PMechaOnRoboSet(Get)_1のエリアにいるときは、PMechaOnRoboSet_1へ
2882 '・PMechaOnRoboSet
2883 '・PMechaOnRoboSet_1
2884 '・PMechaOnRoboGet
2885 '・PMechaOnRoboGet_1
2886 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
2887     PActive = P_Curr                    '現在位置を取得
2888     If (PActive.X >= -150) And (PActive.X <= -60) Then
2889         If (PActive.Y >= 540) And (PActive.Y <= 570) Then
2890             If (PActive.Z >= 290) And (PActive.Z <= 320) Then
2891                 Mvs PMechaOnRoboSet_1
2892                 Dly 1.0
2893             EndIf
2894         EndIf
2895     EndIf
2896 '
2897 'PMechaOnRoboSet(Get)_1～PMechaOnRoboSet(Get)_2のエリアにいるときは、PMechaOnRoboSet_2へ
2898 '・PMechaOnRoboSet_1
2899 '・PMechaOnRoboSet_2
2900 '・PMechaOnRoboGet_1
2901 '・PMechaOnRoboGet_2
2902 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
2903 '    PActive = P_Curr                    '現在位置を取得
2904 '    If (PActive.X >= -90) And (PActive.X <= 50) Then
2905 '        If (PActive.Y >= 300) And (PActive.Y <= 570) Then
2906 '            If (PActive.Z >= 290) And (PActive.Z <= 430) Then
2907 '                Mov PMechaOnRoboSet_2
2908 '                Dly 1.0
2909 '            EndIf
2910 '        EndIf
2911 '    EndIf
2912 '
2913 '上空退避
2914     PActive = P_Curr
2915     Pmove = PActive
2916     Pmove.Z = 500           '上空退避する一律の高さ
2917     If PActive.X < -400 Then
2918         Pmove.Z =290        '金具(F)受け取り位置上に腕を伸ばしているときは500まで上げられない為、例外処置
2919     EndIf
2920     If PActive.X > 400 Then
2921         Pmove.Z =400        'パレット上に腕を伸ばしているときは500まで上げられない為、例外処置
2922     EndIf
2923     If PActive.Z < Pmove.Z Then
2924         Mvs Pmove
2925     EndIf
2926     Dly 1.0
2927 'J1軸以外を退避ポジションへ移動
2928     JActive = J_Curr
2929     Jmove = JTaihi
2930     Jmove.J1 = JActive.J1        'J1軸は現在値を使用し、JTaihiのポーズを取る
2931     Jmove.J6 = JActive.J6        'J6軸は現在値を使用し、JTaihiのポーズを取る
2932     Mov Jmove
2933     Dly 1.0
2934 'J1軸のみを退避ポジションへ移動
2935     Mov JTaihi
2936     Dly 1.0
2937 'イニシャルポジションへ移動
2938     Mov PInitialPosition
2939     Cmp Off
2940     Ovrd 100
2941 ' ねじロボを初期位置に戻すために強制的に自動運転開始         '2022/04/20 ファンクションの外へ移動 渡辺
2942 '    If M_In(11856) = 0 Then                 ' 停止中のみ
2943 '        M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
2944 '        MRet = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
2945 '        If MRet = 0 Then
2946 '        Else
2947 '            M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
2948 '        EndIf
2949 '    EndIf
2950     fErrorProcess(11,253,281,0)
2951 *RecoveryEnd
2952     Exit Function
2953 FEnd
2954 '
2955 '
2956 '■fnAutoScreenComment
2957 ''' <summary>
2958 ''' メイン画面の動作状況表示
2959 ''' コメントD1005の設定
2960 ''' </summary>
2961 '''<param name="McommentD1005%">コメントID</param>
2962 ''' <remarks>
2963 ''' Date   : 2021/07/07 : M.Hayakawa
2964 ''' </remarks>
2965 Function fnAutoScreenComment(ByVal McommentD1005%)
2966     M_Out16(12576) = McommentD1005%
2967 FEnd
2968 '
2969 '■fnRoboPosChk
2970 ''' <summary>
2971 ''' 最後に終了したロボットポジションの確認
2972 ''' </summary>
2973 '''<param name="MINNumber%">入力番号</param>
2974 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2975 '''<param name="MTimeCnt&">タイムアウト時間</param>
2976 ''' PLCに保続した番号を読込み、確認
2977 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2978 '''<returns>整数 0:タイムアウト 1:OK</returns>
2979 ''' <remarks>
2980 ''' Date   : 2021/07/07 : M.Hayakawa
2981 ''' </remarks>
2982 Function M% fnRoboPosChk
2983     fnRoboPosChk = 0
2984     MRet = fnStepRead()
2985     '初期位置でないと判断した場合
2986     'ウィンド画面切換え
2987     If MRBTOpeGroupNo > 5 Then
2988         '下記キー待ちの継続に反応させないため
2989         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2990         Dly 0.2
2991         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2992         Dly 1.5
2993         '
2994         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2995         '
2996         MLoopFlg% = 1
2997         While MLoopFlg% = 1
2998             '
2999             '
3000             MKeyNumber% = fnKEY_WAIT()
3001             Select MKeyNumber%
3002                 Case Is = MAbout%       '停止
3003                     M_20# = MAbout%
3004                     MLoopFlg% = -1
3005                     Break
3006                 Case Is = MNext%        '次へ
3007                     'MLoopFlg% = -1
3008                     Break
3009                 Case Is = MContinue%    '継続
3010                     M_20# = MContinue%
3011                     MLoopFlg% = -1
3012                     Break
3013                 Default
3014                     Break
3015             End Select
3016         WEnd
3017     EndIf
3018     '
3019     If M_20# = MContinue% Then                              '継続ボタンが押された場合
3020         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
3021         Ovrd 5                                   '低速オーバーライド値設定
3022         Select MRBTOpeGroupNo
3023             Case Is = 5                          '何もしない
3024                 Break
3025             Case Is = 10                         '初期位置へ戻す
3026                 'Mov PTEST001
3027                 Break
3028             Case Is = 15                         '初期位置へ戻す
3029                 'Mov PTEST002
3030                 Dly 0.5
3031                 'Mov PTEST001
3032                 Dly 0.5
3033                 Break
3034             Default
3035                 Break
3036         End Select
3037         '
3038         Ovrd M_NOvrd                            'システムの初期値を設定
3039         M_Out(12364) = 1                        'toPLC_データ保存ON
3040         MRBTOpeGroupNo = 5
3041         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
3042         Dly 1.0
3043         M_Out(12364) = 0                        'toPLC_データ保存OFF
3044         fnRoboPosChk = 1                        '初期位置動作実行
3045         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
3046     EndIf
3047     Exit Function
3048 FEnd
3049 '
3050 '■frInCheck
3051 ''' <summary>
3052 ''' センサーINチェック
3053 ''' </summary>
3054 '''<param name="MINNumber%">入力番号</param>
3055 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3056 '''<param name="MTimeCnt&">タイムアウト時間</param>
3057 '''<returns>整数 0:タイムアウト 1:OK</returns>
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
3077 'ねじ締め機通信確認
3078 '
3079 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3080 'fScewTcomChk = 0　：正常終了
3081 '          　　 -1 ：異常終了
3082 '-----------------------------------------------
3083 Function M% fScewTcomChk
3084 *ReCheckScewTcomChk
3085     fScewTcomChk = 0
3086     '通信確認送信
3087     M_Out(MOUT_ScwT_ComChk%) = MOn%
3088     '通信確認受信待機
3089 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3090     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3091     '通信確認送信終了
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
3103 'ねじ締め開始送信
3104 '
3105 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3106 'fScewTStart = 0　：正常終了
3107 '          　　-1 ：異常終了
3108 '-----------------------------------------------
3109 Function M% fScewTStart
3110     fScewTStart = 0
3111     nRet% = 0
3112     'ねじ締め開始待機を受信
3113 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3114     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3115     If MRtn = 0 Then nRet% = -1
3116     If MRtn = 0 Then GoTo *ScrewStartERROR      '開始できなかった場合ジャンプ
3117     Dly 0.1
3118     'ねじ締め開始受信を送信
3119     M_Out(MOUT_ScwT_ST%) = MOn%
3120     Dly 0.5
3121     'Wait M_In(MTEST_KEY%) = MOn%
3122     'ねじ締め開始送信終了
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
3133 'ねじ締め完了受信
3134 '
3135 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3136 'fScewTcomChk = 0　：正常終了
3137 '          　 　-1 ：異常終了
3138 '-----------------------------------------------
3139 Function M% fScewTFinish
3140 *ReCheckScewTFinish
3141     fScewTFinish = 0
3142     'ねじ締め完了待機を受信
3143 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3144     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3145     If MRtn = 0 Then
3146         fScewTFinish = -1
3147     EndIf
3148     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3149     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3150     Dly 0.1
3151     'ねじ締め完了受信を送信
3152     M_Out(MOUT_ScwT_FinOK%) = MOn%
3153     Dly 0.5                          'とりあえず保持時間0.5msec
3154     'ねじ締め開始送信終了
3155     M_Out(MOUT_ScwT_FinOK%) = MOff%
3156     'Wait M_In(MTEST_KEY%) = MOn%
3157     '
3158 *ScewTFinish_ErrEnd
3159 FEnd
3160 '
3161 '
3162 '-----------------------------------------------
3163 '
3164 '条件xx停止受信
3165 '
3166 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3167 'fScewTCaseStop = 0　：正常終了
3168 '          　   　-1 ：異常終了
3169 '-----------------------------------------------
3170 Function M% fScewTCaseStop(ByVal MCase%())
3171 *ReCheckScewTCaseStop
3172     fScewTCaseStop = 0
3173     '条件xx停止を受信
3174     Wait M_In(MCase%(1)) = MOn%
3175     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3176     If MRtn = 0 Then
3177         fScewTCaseStop = -1
3178     EndIf
3179     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3180     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3181     Dly 0.1
3182     '条件xx停止受信を送信
3183     M_Out(MCase%(2)) = MOn%
3184     Dly 0.5                          'とりあえず保持時間0.5msec
3185     'ねじ締め開始送信終了
3186     M_Out(MCase%(2)) = MOff%
3187 *ScewTCaseStop_ErrEnd
3188     '
3189 FEnd
3190 '
3191 '
3192 '
3193 '■fScrewTighenRoboCheck
3194 '<summary>
3195 'ねじロボ監視
3196 '</summary>
3197 '<param name = "MStopNum%"> 停止番号</param>
3198 '<returns>整数 0:ねじロボ異常終了 1:OK </returns>
3199 '<make>
3200 '2021/12/2 中村天哉
3201 '</make>
3202 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3203     fnAutoScreenComment(503)    '状態表示[ねじロボ動作終了待ち] 2022/04/26 渡辺
3204     fScrewTighenRoboCheck = 1
3205     MScrewTighenRoboFlg% = 1    'フラグの初期化
3206     MCheck% = 0
3207     While MScrewTighenRoboFlg% = 1
3208         MCheck% = M_In16(11904)
3209         If M_In(MStopNum%) = 1 Then '停止位置まで来たら
3210             MScrewTighenRoboFlg% = 0 '関数を抜ける
3211             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
3212         EndIf
3213         If MCheck% <> 0 Then
3214             fScrewTighenRoboError(MCheck%)
3215             Select M_20#
3216                 Case MAbout%            '停止が押された場合
3217                     M_Out(12869) = 1 Dly 1.0
3218                     MScrewTighenRoboFlg% = 0
3219                     fScrewTighenRoboCheck = 0   '異常終了
3220                     Break
3221                 Case MNgProcess%        'NGが押された場合
3222                     M_Out(12873) = 1 Dly 1.0
3223                     MScrewTighenRoboFlg% = 0
3224                     fScrewTighenRoboCheck = 0   '異常終了
3225                     Break
3226                 Case MContinue%             'リトライが押された場合
3227                     M_20# = MClear%         'M_20#初期化
3228                     M_Out(12871) = 1 Dly 1.0
3229                     Break
3230                 Case MNext%                 '次へが押された場合
3231                     M_20# = MClear%         'M_20#初期化
3232                     M_Out(12874) = 1 Dly 1.0
3233                     Break
3234             End Select
3235             Dly 0.5
3236         EndIf
3237     WEnd
3238 FEnd
3239 '■fScrewTighenRoboError
3240 '<summary>
3241 'ねじロボエラー処理
3242 '</summary>
3243 '<param name = "ErrorCode%"> エラー番号</param>
3244 '<make>
3245 '2021/12/2 中村天哉
3246 '</make>
3247 Function fScrewTighenRoboError(ErrorCode%)
3248     MCommentD1001 = ErrorCode% + 300
3249     fErrorProcess(11,MCommentD1001,0,0)
3250 FEnd
3251 '
3252 '■fErrorProcess
3253 '<summary>
3254 'エラー処理
3255 '</summary>
3256 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
3257 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
3258 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
3259 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
3260 '<make>
3261 '2021/11/5 中村天哉
3262 '</make>
3263 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3264     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
3265     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
3266     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
3267     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
3268     MKeyNum% = 0
3269 *RETRY_ERR_PROCESS
3270      M_20# = MClear%     '初期化
3271 '        'エラー処理記述
3272         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3273 '        'GOT KEY入力待ち
3274         MKeyNum% = fnKEY_WAIT()
3275 '        '
3276         If MKeyNum% = MAbout% Then   '停止を選択した場合
3277             M_20# = MAbout%            'M_20# プログラム間共通外部変数
3278             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3279             Break
3280          '
3281         ElseIf MKeyNum% = MContinue% Then   '継続を選択した場合
3282             M_20# = MContinue%            'M_20# プログラム間共通外部変数
3283             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3284         '
3285         ElseIf MKeyNum% = MNext% Then   '次へを選択した場合
3286             M_20# = MNext%            'M_20# プログラム間共通外部変数
3287             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3288          '
3289         ElseIf MKeyNum% = MNgProcess% Then   '停止を選択した場合
3290             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
3291             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3292             Break
3293         '
3294         EndIf
3295         '
3296         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3297 FEnd
3298 '
3299 '■fnTorqueCheck
3300 ''' <summary>
3301 ''' トルクチェック動作用のメイン
3302 ''' </summary>
3303 ''' <remarks>
3304 ''' Date   : 2021/12/21 : H.AJI
3305 ''' </remarks>'
3306 Function M% fnTorqueCheck
3307     'トルクチェック中送信  搬送系停止
3308     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
3309     '
3310     fnTorqueCheck = 0
3311     Ovrd 20
3312     Mov PInitialPosition              '初期位置移動
3313     Accel 100 , 20
3314     Mvs PHandChange                   'ハンド交換位置
3315     Accel 100 , 100
3316     Ovrd 100
3317     '下記キー待ちの継続に反応させないため
3318     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3319     Dly 0.2
3320     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3321     '
3322     'M6340  トルクチェック受信
3323     'Dly 5.0
3324     M_Out(12340) = 1          'トルクチェック受信 M6340
3325     Dly 1.0
3326     M_Out(12340) = 0
3327     '
3328     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3329     M_Out(12835) = 1                         'ねじロボトルクチェック画面切替
3330    Wait M_In(11843) = 1                         'ねじロボトルクチェック画面切替
3331     M_Out(12835) = 0                         'ねじロボトルクチェック画面切替完了
3332     '
3333     '
3334     MLoopFlg = 1
3335     While MLoopFlg = 1
3336         '
3337 '        Mov PInitialPosition              '初期位置移動
3338         '
3339         MKeyNumber = fnKEY_WAIT()
3340         Select MKeyNumber
3341             Case Is = 1           '停止
3342                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3343                 Dly 1.0
3344                 M_Out(12343) = 0
3345                 Ovrd 20
3346                 'Mov PTicketRead_1
3347                 M_Out(12840) = 1          'トルクチェック終了
3348                 Wait M_In(11859) = 1      'ねじロボからの終了
3349                 M_Out(12840) = 0          'トルクチェック終了
3350                 Ovrd 100
3351                 M_20# = 1
3352                 MLoopFlg = -1
3353                 Break
3354             Case Is = 2           '次へ
3355                 Break
3356             Case Is = 3           '継続
3357                 Break
3358             Case Is = 4           'トルクチェック開始
3359                 M_Out(12342) = 1          'トルクチェック開始要求受信 M6342
3360                 Dly 1.0
3361                 M_Out(12342) = 0
3362                 If M_In(11862) = 1 Then             'トルクチェッカー確認
3363                     fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3364                     MRet = fnScrewMTorque()           'ねじロボ用トルクチェック
3365                 EndIf
3366                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3367                 'MRet = fnMoveTorquePosi()
3368                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3369                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3370                 Break
3371             Default
3372                 Break
3373         End Select
3374     WEnd
3375     '
3376     'トルクチェック中停止送信
3377     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3378     '
3379     'ロボットの位置を元に戻す
3380     Mvs PInitialPosition            'イニシャルポジション
3381     '
3382     '
3383  FEnd
3384  '
3385 '
3386 '
3387 '---------------------------
3388 '
3389 '    メイン画面の表示、非表示設定
3390 '         コメントD1001, D1002, D1003の設定
3391 '           MWindReSet = 0     画面非表示
3392 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3393 '           MWindErrScr = 10    エラー画面 D1001, D1002
3394 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3395 '
3396 '---------------------------
3397 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3398     fnMainScreenOpen = 0
3399     '
3400    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3401         M_Out16(12480) = MCommentD1001            'D1001 コメント
3402     EndIf
3403     '
3404     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3405         M_Out16(12496) = MCommentD1002            'D1002 コメント
3406     EndIf
3407     '
3408     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3409         M_Out16(12512) = MCommentD1003            'D1003 コメント
3410     EndIf
3411     '
3412     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3413     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3414     Dly 0.5
3415     M_Out(12362) = 0                         'ウィンド画面設定
3416 FEnd
3417 '
3418 '■Main
3419 ''' <summary>
3420 ''' トルクチェック実動作
3421 ''' </summary>
3422 ''' <remarks>
3423 ''' Date   : 2021/12/21 : H.AJI
3424 ''' </remarks>'
3425 Function M% fnScrewMTorque
3426     fnScrewMTorque = 0
3427     M_Out(12838) = 1                         'トルクチェック開始1
3428     Wait M_In(11857) = 1                     '受信完了
3429     M_Out(12838) = 0                         'トルクチェック開始1
3430     Dly 2.0
3431 FEnd
3432 '
3433 '
3434 '----------------------------------------------------------------
3435 'fTimeOutJudge
3436 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3437 '引数
3438 'Address% = 監視アドレス番号
3439 'JudgeFlg% = 対象アドレスの正常終了時の値
3440 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3441 '戻り値 = 0 エラー
3442 '         1 正常終了
3443 '         2 リトライ
3444 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3445 '作成日
3446 '2022/9/20 中村
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
3468 '■Main
3469 ''' <summary>
3470 ''' トルクチェック実動作
3471 ''' </summary>
3472 ''' <remarks>
3473 ''' Date   : 2021/12/21 : H.AJI
3474 ''' </remarks>'
3475 Function M% fnMoveTorquePosi
3476      fnMoveTorquePosi = 0
3477      Ovrd 50
3478     'Mov PTorquePosi000 'トルクチェック回避位置へ移動
3479      Mov PTorqueCheck_1 'トルクチェックメーター上空へ移動
3480     'Mov PTorquePosi020 'トルクチェックビットジョイント上空
3481     '
3482     '
3483      '以下は阿部さんが作成したトルクチェックプラグラム
3484     '
3485     Spd M_NSpd
3486 '-------------      ドライバーRST
3487     M_Out(12240)=0     'ドライバーOFF CCW
3488     M_Out(12241)=0     'ドライバーOFF CW
3489     M_Out(12242)=0     'ドライバー解除 C1
3490     M_Out(12243)=0     'ドライバー解除 C2
3491     M_Out(12245)=0     'プログラム解除 F1/プログラム2
3492 '---------------------------------------
3493 '---------------------------------------
3494     Fsc Off            '力覚センサ　Off  STEP1は不要
3495 '--------------------------------------------------------------
3496 '--------------------------------------------------------------
3497 '[P-11]
3498 '--------------------------------------------------------------   【トルクチェック 0.4N - P11】
3499     Mov PTorqueCheck, -50                     ' トルク-1　置き位置上空 50mm へ移動
3500    'Mov PTorquePosi020, -10                    ' トルク-1　置き位置上空 10mm へ移動
3501     Dly 0.1
3502 '-----------------------
3503    'Cnt 0                           'Cnt動作-2　終了
3504 '-----------------------
3505     Mov PTorqueCheck , -5                      'トルク-1　置き位置上空 5mm へ移動
3506     Dly 0.2
3507 '-----------------------
3508     M_Out(12242)=1                   'ドライバーセット C1
3509     Dly 0.1
3510     M_Out(12243)=1                   'ドライバーセット C2 (バンク3)
3511     Dly 0.1
3512     M_Out(12245)=1                   'プログラム2セット F1  Mネジ
3513     Dly 0.1
3514     'M_Out(12241)=1                   'ドライバーON  CW
3515    M_Out(12241)=0                   'ドライバーOFF  CW
3516     'Dly 0.1
3517 '--------------------------------
3518     Ovrd 40
3519    'Dly 0.1
3520 '--------------------------------  ネジ締め速度設定
3521     Spd 14                            'ライド 100-40 100% :Spd 12
3522     Dly 0.1
3523 '--------------------------------
3524 '--------------------------------
3525 '---------------------------------【ねじ締め動作】
3526 '
3527     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '移動中エラー検出
3528    Mvs PTorqueCheck               'トルクチェック位置へ移動
3529     Dly 0.3                          '動作安定待ち
3530    M_Out(12241)=1                   'ドライバーON  CW
3531 '
3532     Wait M_In(11584)=1                '完了/エラー検出
3533     Dly 0.1
3534     Spd M_NSpd
3535    'Ovrd 20
3536     If M_In(11256)=1 Then *LBL1       'ネジトータルエラー検出
3537     Wait M_In(11257)=1                'ネジ完了SC
3538 '---------------------------------
3539     Dly 0.1
3540     M_Out(12241)=0                    'ドライバーOFF CW
3541     Dly 0.1
3542     M_Out(12242)=0                    'ドライバー解除 C1
3543     Dly 0.1
3544     M_Out(12243)=0                    'ドライバー解除 C2 (バンク3)
3545     Dly 0.1
3546     M_Out(12245)=0                    'プログラム2解除 F1
3547 '--------------------------------------------------------------   【トルクチェック 0.4N - P11ここまで】
3548 '
3549     Mvs PTorqueCheck,-60                       'あえてmov から変更
3550     Dly 0.1
3551 '--------------------------------------------------------------
3552    'Ovrd 80
3553 '--------------------------------------------------------------
3554 '---------------------------------------
3555 '---------------------------------------
3556 '---------------------------------------エラー離脱処理
3557    *LBL1
3558    Fsc Off            '力覚センサ　Off   *STEP1は不要
3559    Mvs ,-100
3560    M_Out(12241)=0     'ドライバーOFF CW
3561    Dly 0.1
3562    M_Out(12242)=0     'ドライバー解除 C1
3563    Dly 0.1
3564    M_Out(12243)=0     'ドライバー解除 C2 (バンク3)
3565    Dly 0.1
3566    M_Out(12245)=0     'プログラム解除 F1
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
3577 ''■Main
3578 ''' <summary>
3579 ''' 組立動作用のメイン
3580 ''' </summary>
3581 ''' <remarks>
3582 ''' Date   : 2021/07/07 : M.Hayakawa
3583 ''' </remarks>'
3584 Function Main
3585     MopeNo = M_21#         '外部変数にて動作番号代入
3586     '
3587     If M_Svo=0 Then
3588         Servo On
3589     EndIf
3590     Wait M_Svo=1
3591 '組立スタート日付時刻要求パルスON (別スロットの8から要求に変更）
3592 '    M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3593 'パトライト操作
3594     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3595     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3596     '
3597     M_20# = 0                                   'KEY入力初期化
3598 '    M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化(位置移動1/18中村)
3599     MRet% = 0
3600 '初期位置の確認と移動
3601 '
3602 '復帰動作　実行・未実行判別      2022/03/22 渡辺 作成
3603     PActive = P_Curr                    '現在位置を取得
3604     MRecoveryPass% = 0
3605     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3606         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3607             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3608                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3609             EndIf
3610         EndIf
3611     EndIf
3612     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3613         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3614             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3615                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3616             EndIf
3617         EndIf
3618     EndIf
3619     If (PActive.X <= PMechaOnJigGet_3.X + 1.0) And (PActive.X >= PMechaOnJigGet_3.X -1.0) Then
3620         If (PActive.Y <= PMechaOnJigGet_3.Y + 1.0) And (PActive.Y >= PMechaOnJigGet_3.Y -1.0) Then
3621             If (PActive.Z <= PMechaOnJigGet_3.Z + 1.0) And (PActive.Z >= PMechaOnJigGet_3.Z -1.0) Then
3622                 MRecoveryPass% = 1       'DVD取り位置上空位置は復帰動作パス
3623             EndIf
3624         EndIf
3625     EndIf
3626     If MRecoveryPass% = 0 Then
3627        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3628     EndIf
3629 '
3630 ' ねじロボを初期位置に戻すために強制的に自動運転開始        'fnInitialZoneBの外へ移動 2022/04/20 渡辺
3631     If MopeNo <> 2 And M_In(MIN_TorqueCheck%) <> 1 Then       'トルクチェックの時は以下を実行しない 2022/04/21 渡辺
3632         If M_In(11856) = 0 Then                 ' 停止中のみ
3633             fnAutoScreenComment(501)            ' 状態表示[ネジ締め機自動運転開始中] 2022/04/25 渡辺
3634             M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
3635             MRet% = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
3636             If MRet% = 0 Then
3637             Else
3638                 M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
3639             EndIf
3640         EndIf
3641     EndIf
3642 '
3643 '    MRet% = fnRoboPosChk()
3644 '    If MRet% = 1 Then                           '初期位置の動作を行った場合     '2022/04/20 コメントアウト 渡辺
3645 '        fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  'ウィンド画面
3646 '        MKeyNumber% = fnKEY_WAIT()
3647 '        Select MKeyNumber%
3648 '            Case Is = MAbout%       '停止
3649 '                M_20# = MAbout%
3650 '                MLoopFlg% = -1
3651 '                Break
3652 '            Case Is = MNext%        '次へ
3653 '                'MLoopFlg = -1
3654 '                Break
3655 '            Case Is = MContinue%    '継続
3656 '                M_20# = MContinue%
3657 '                MLoopFlg% = -1
3658 '                Break
3659 '            Default
3660 '                Break
3661 '        End Select
3662 '    EndIf
3663     '
3664     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3665         M_Out(12364) = 1            'toPLC_データ保存ON
3666 'トルクチェック
3667         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3668             MRet% = fnTorqueCheck()
3669             Break
3670         Else
3671 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認'12/21コメントアウト(中村)
3672 '                MRtn = InspInit()               '画像処理初期化処理
3673 '            EndIf
3674             '
3675            M_20# = MClear%                    '初期化
3676 '組立開始
3677             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3678                 'MRet% = fnAssyStart()  '暫定コメントアウト
3679                 fnAssyStart()
3680             Else
3681                 M_20# = MPass%
3682             EndIf
3683             M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化(位置移動1/18中村)
3684 '組立終了日付時刻
3685             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3686             Wait M_In(11572) = 1            '日付取得完了
3687             Dly 0.1
3688             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3689 'リフターユニットへのOUT
3690             '  KEY入力が何もない場合 OKと判断
3691             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3692             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3693 'OK/NGフラグ出力
3694             If M_20# = MAssyOK% Or M_22# = MIrregular% Then
3695                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3696             ElseIf M_20# = MPass% Then
3697                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3698             EndIf
3699 'PIASに組立完了書込み
3700             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3701                 If M_20# = MPass% Then
3702                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3703                 Else
3704                     'KEY入力がNGの場合
3705                     If M_20# = MNgProcess% Then
3706                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3707                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3708                         MRet% = fnPiasWrite(MNG%)
3709                        nAssyNgQty = nAssyNgQty + 1
3710                     EndIf
3711                     '
3712                     'KEY入力が何もない場合 OKと判断(0からMAssyOK%へ変更1/12中村)
3713                     If M_20# = MAssyOK% Or M_22# = MIrregular% Then
3714                             '-----------------------
3715                             'D732 -> D2600 コピー要求
3716                             M_Out(12566) = 1
3717 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3718                             M_Out(12566) = 0
3719                             '
3720                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3721                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3722                             '基板番号照合(PPは未使用）
3723 '                            MRet% = fnPCBNumberCheck()
3724                         Else
3725                             MRet% = 1
3726                         EndIf
3727                         '
3728                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3729                             If M_20# <> MAbout% Then
3730                                 '工程履歴OK書き込み
3731                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3732                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3733                                 MRet% = fnPiasWrite(MOK%)
3734                                 nAssyOkQty = 0
3735                                 nAssyOkQty = nAssyOkQty + 1
3736                             Else
3737                                 nAssyOkQty = nAssyOkQty + 1
3738                             EndIf
3739                         EndIf
3740                     EndIf
3741 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3742 '                    MRet% = fnPiasWrite(MOK%)
3743                 EndIf
3744             Else
3745                 nAssyOkQty = nAssyOkQty + 1
3746             EndIf
3747             '
3748             '組立終了日付時刻解除
3749             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3750             '投入数、組立OK数、組立NG数書込み
3751 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3752             '
3753 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認'コメントアウト12/21(中村)
3754 '                '画像処理終了処理
3755 '                MRtn = InspQuit()
3756 '            EndIf
3757         EndIf
3758         M_Out(12364) = 0                          'toPLC_データ保存OFF
3759     EndIf
3760 'パトライト操作
3761     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3762     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3763 'GOT表示
3764     fnAutoScreenComment(93)  'AUTO画面 工程完了
3765 FEnd
3766 End
3767 '
3768 '
3769 'おまじないコメント
3770 '絶対削除するな
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
