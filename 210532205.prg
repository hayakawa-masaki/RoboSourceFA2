1 ' ===================================
2 '
3 '  21053001 STEP5 Assy5プログラム
4 '
5 ' 作成者：M.Hayakawa
6 ' 作成日：2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1から流用
8 ' ===================================
9 '===== <Insight定数> =====
10 '===== <Insight変数定義> =====
11 Dim PInspPosition(30)               '画像処理Function引渡し用位置変数
12 Dim MInspGroup%(30)                 '画像処理Function引渡し用変数
13 Def Inte MIN_IS_Ready               '【入力IO】Insight準備OK
14 Def Inte MIN_IS_JobLoadOK           '【入力IO】Insightジョブロード正常終了
15 Def Inte MIN_IS_JobLoadNG           '【入力IO】Insightジョブロード異常終了
16 Def Inte MIN_IS_InspGSetOK          '【入力IO】Insight検査グループ番号設定正常終了
17 Def Inte MIN_IS_InspGSetNG          '【入力IO】Insight検査グループ番号設定異常終了
18 Def Inte MIN_IS_InspOK              '【入力IO】Insight検査OK
19 Def Inte MIN_IS_InspNG              '【入力IO】Insight検査NG
20 Def Inte MIN_IS_InspErr             '【入力IO】Insight検査異常終了
21 Def Inte MIN_IS_InspCapDone         '【入力IO】Insight検査画像取込完了
22 '
23 Def Inte MIN_IS_ErrNum              '【入力IO】Insight処理エラー番号取得開始アドレス(16bit)
24 'Output Signal
25 Def Inte MOUT_IS_JobLoadReq         '【出力IO】Insight JOBロード要求
26 Def Inte MOUT_IS_InspGSetReq        '【出力IO】Insight 検査グループ番号設定要求
27 Def Inte MOUT_IS_Insp               '【出力IO】Insight 検査実行要求
28 '
29 Def Inte MOUT_IS_JobNum             '【出力IO】Insight JOB番号設定開始アドレス(16bit)
30 Def Inte MOUT_IS_InspGNum           '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
31 '
32 Def Inte MOUT_InspErrNum            '【出力IO】検査実行エラー番号開始アドレス(16bit)
33 Def Inte MOUT_InspNGStepNum         '【出力IO】検査実行NGStep番号開始アドレス(16bit)
34 '作業用変数
35 Def Inte MInspErrNum                '検査実行エラー番号
36 Def Inte MInspNGStepNum             '検査実行NGStep番号
37 Def Inte MRtn                       'Function戻り値取得用
38 Def Inte MRtn2                      'Function戻り値取得用
39 Def Inte MRet3                      'Function戻り値取得用
40 Def Inte MGRtn                      'Function戻り値取得用 ネジ供給機
41 Def Inte MInspErrNumSub             '検査実行エラー番号sub　20190820追加
42 Def Inte MovrdA                     'ネジ締めOvrd 可変用
43 Def Float MSpdA                     'ネジ締めSpd　可変用
44 Def Pos PTemp                       'ネジ締め上空位置計算用
45 '===== <Insight変数設定> =====
46 MIN_IS_Ready%        =   11380      '【入力IO】Insight準備OK
47 MIN_IS_JobLoadOK%    =   11381      '【入力IO】Insightジョブロード正常終了
48 MIN_IS_JobLoadNG%    =   11382      '【入力IO】Insightジョブロード異常終了
49 MIN_IS_InspGSetOK%   =   11383      '【入力IO】Insight検査グループ番号設定正常終了
50 MIN_IS_InspGSetNG%   =   11384      '【入力IO】Insight検査グループ番号設定異常終了
51 MIN_IS_InspOK%       =   11385      '【入力IO】Insight検査OK
52 MIN_IS_InspNG%       =   11386      '【入力IO】Insight検査NG
53 MIN_IS_InspErr%      =   11387      '【入力IO】Insight検査異常終了
54 MIN_IS_InspCapDone%  =   11388      '【入力IO】Insight検査画像取込完了
55 MIN_IS_ErrNum%       =   11408      '【入力IO】Insight処理エラー番号開始アドレス(16bit)
56 'Output Signal
57 MOUT_IS_JobLoadReq%  =   12370      '【出力IO】Insight JOBロード要求
58 MOUT_IS_InspGSetReq% =   12371      '【出力IO】Insight 検査グループ番号設定要求
59 MOUT_IS_Insp%        =   12372      '【出力IO】Insight 検査実行要求
60 MOUT_IS_JobNum%      =   12384      '【出力IO】Insight JOB番号設定開始アドレス(16bit)
61 MOUT_IS_InspGNum%    =   12400      '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
62 MOUT_InspErrNum%     =   12416      '【出力IO】検査実行エラー番号開始アドレス(16bit)
63 MOUT_InspNGStepNum%  =   12432      '【出力IO】検査実行NGStep番号開始アドレス(16bit)
64 '===== <電ドラ変数定義> =====
65 X20_Driver=11248                    '電ドラステイタス1　Driver Status 1
66 X21_Driver=11249 '電ドラステイタス2  Driver Status 2
67 X22_Driver=11250 '電ドラステイタス3  Driver Status 3
68 X23_Driver=11251 '電ドラステイタス4  Driver Status 4
69 X24_Driver=11252 '電ドラエラーメッセージ1 Driver Error E1
70 X25_Driver=11253 '電ドラエラーメッセージ2 Driver Error E2
71 X26_Driver=11254 '電ドラエラーメッセージ3 Driver Error E3
72 X27_Driver=11255 '電ドラエラーメッセージ4 Driver Error E4
73 X28_Driver=11256 '電ドラトータルエラーシグナル Total Error
74 X29_Driver=11257 '電ドラ終了シグナル Comlete signal
75 X2A_Driver=11258 '電ドラエラーメッセージ5 Driver Error E5
76 '11584   'toRBトルクドライバ-COMP_ERR送信
77 Y60_Driver=12240 '電ドラ半時計回り CCW
78 Y61_Driver=12241 '電ドラ時計回り CW
79 Y62_Driver=12242 'バンクセッティング BANK C1
80 Y63_Driver=12243 'バンクセッティング BANK C2
81 Y64_Driver=12244 'バンクセッティング BANK C3
82 Y65_Driver=12245 'プログラムセッティング PRG SET F1
83 Y66_Driver=12246 'プログラムセッティング PRG SET F2
84 Y67_Driver=12247 'プログラムセッティング PRG SET F3
85 X34_ScrewReady1=11259 'ねじっこ1　Read
86 '===== <電ドラ定数> =====
87 Dim PScrewPos(10)       'ネジ締め用Function引数変数
88 Dim PGetScrewPos(10)    'ねじ供給機からねじを得るFunction引数変数
89 Dim PEscapePosi(10)
90 MLoopCnt% = 0'
91 '===== <ロボット定数> =====
92 '===== <ロボット変数定義> =====
93 MRBTOpeGroupNo = 0      'ロボット動作番号初期化
94 MCommentD1001 = 0
95 MCommentD1002 = 0
96 MCommentD1003 = 0
97 MScreenNo = 0
98 '
99 MCommentTSU = 0
100 MCommentTSD = 0
101 'ウィンド画面番号設定
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
114 MClear% = 0        'KEY_のクリア
115 MAbout% = 1        'KEY_停止
116 MNext% = 2         'KEY_次のステップへ移行
117 MContinue% = 3     'KEY_継続 再度同じ動作を行う
118 '
119 Def Inte MNgProcess
120 MNgProcess% = 5      'KEY_NG
121 '
122 MAssyOK% = 6       '組立完了
123 MPass% = 7         '工程パス
124 MPiasNG% = 8       'Pias確認時履歴NG
125 '
126 '初期化用KEY番号   '
127 MRobotInit1% = 11  '初期位置用
128 MRobotInit2% = 12  '初期位置用
129 MRobotInit3% = 13  '初期位置用
130 MRobotInit4% = 14  '初期位置用
131 '
132 MIN_INIT1REQUEST% = 11568 'toRBT_ロボット初期位置1要求
133 MIN_INIT2REQUEST% = 11569 'toRBT_ロボット初期位置2要求
134 MIN_INIT3REQUEST% = 11570 'toRBT_ロボット初期位置3要求
135 MIN_INIT4REQUEST% = 11571 'toRBT_ロボット初期位置4要求
136 '
137 MOUT_INIT1RECIVE% = 12560 'toPLC_ロボット初期位置1受信
138 MOUT_INIT2RECIVE% = 12561 'toPLC_ロボット初期位置2受信
139 MOUT_INIT3RECIVE% = 12562 'toPLC_ロボット初期位置3受信
140 MOUT_INIT4RECIVE% = 12563 'toPLC_ロボット初期位置4受信
141 '
142 MOK% = 1               '各判定用
143 MNG% = 0               '各判定用
144 MTIMEOUT% = -1         '各判定用
145 MJudge% = 0            '判定情報格納用
146 '
147 MRECIVETIME& = 0
148 MSETTIMEOUT10& = 10000&                '10秒設定
149 MSETTIMEOUT03& = 3000&                 '3秒設定
150 MSETTIMEOUT01& = 1000&                 '1秒設定
151 MSETTIMEOUT05& = 5000&                 '5秒設定
152 MSETTIMEOUT009& = 900&                 '0.9秒設定
153 MSETTIMEOUT008& = 800&                 '0.8秒設定
154 MSETTIMEOUT007& = 700&                 '0.7秒設定
155 MSETTIMEOUT006& = 600&                 '0.6秒設定
156 MSETTIMEOUT005& = 500&                 '0.5秒設定
157 MSETTIMEOUT004& = 400&                 '0.4秒設定
158 MSETTIMEOUT003& = 300&                 '0.3秒設定
159 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
160 MIN_PIAS_ComOK% = 11552                'PC通信OK
161 MIN_PIAS_ComTimeOut% = 11576           'PC通信確認タイムアウト
162 MIN_PIAS_ComNG% = 11553                'PC通信NG
163 MOUT_PIAS_ComCheck% = 12544            'PC通信確認要求
164 MOUT_PIAS_Missing_Process% = 12546     '工程抜け確認要求
165 MIN_PIAS_ModelTypeNG% = 11554          'モデル仕向NG
166 MIN_PIAS_ProcessHistryNG% = 11555      '前工程履歴NG
167 MIN_PIAS_ProcessHistryOK% = 11556      '前工程履歴OK
168 MIN_PIAS_ProcessHistryErr% = 11557     '工程履歴処理エラー
169 MIN_PIAS_MyProcessComp% = 11573        '自工程履歴あり
170 MIN_PIAS_ProcessHistryTimeOut% = 11578 '工程履歴タイムアウト
171 MOUT_OKNG% = 12549                     'PLC OUT でOK=1, NG=0 出力
172 '
173 MOUT_PiasPCBNumberCheck = 12557        '基板番号照合
174 MIN_PiasPCBNumberOK% = 11566          '基板番号OK
175 MIN_PiasPCBNumberNG% = 11565          '基板番号NG
176 MIN_PiasPCBNumberErr% = 11567         '基板番号処理エラー
177 '
178 MOUT_PiasAssyResultOK% = 12549    '組立OK
179 MOUT_PiasAssyResultNG% = 12550    '組立NG
180 MOUT_PiasAssyResultWr% = 12548    '工程履歴書き込み
181 '
182 MIN_PiasProcessNG% = 11559        '工程履歴処理NG
183 MIN_PiasProcessOtherErr% = 11560  '工程履歴処理エラー(なんかのトラブル)
184 MIN_PiasProcessOK% = 11558        '工程履歴処理OK
185 '
186 MIN_Insight_Use% = 11369               '画像確認ON
187 MIN_TorqueCheck% = 11348               'トルクチェック
188 '
189 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT操作権
190 MOUT_RED_LIGHT% = 12356            'PATLIGHT 赤 点灯
191 MOUT_RED_FLASH% = 12357            'PATLIGHT 赤 点滅
192 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT 黄 点灯
193 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT 黄 点滅
194 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT 青 点灯
195 MOUT_GREEN_FLASH% = 12361          'PATLIGHT 青 点滅
196 '
197 MOUT_ST_DATETIME% = 12551          '組立開始日付時刻
198 MOUT_ED_DATETIME% = 12552          '組立終了日付時刻
199 '
200 MOUT_TORQUE_CHECK% = 12367         'PLCへトルクチェック中を送信
201 '
202 MIN_ASSY_CANCEL% = 11366           '組立を行うかのフラグ
203 '
204 MLoopFlg% = 0                      'KEY入力後のOK or NG内容
205 MopeNo% = 0
206 MRtn% = 0
207 MRet = 0
208 MRet3% = 0
209 '
210 Def Inte MInputQty          '投入数 演算変数
211 Def Inte MAssyOkQty         '組立ＯＫ数 演算変数
212 Def Inte MAssyNgQty         '組立ＮＧ数 演算変数(未使用)
213 Def Inte MSuctionErrQty     '吸着エラー数 2022/04/27 渡辺
214 Def Inte MScrewNo
215 Def Inte MReTry
216 '===== <IO変数定義> =====
217 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
218 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
219 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
220 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
221 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
222 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
223 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
224 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
225 '
226 Def Inte Y6A_VV1            ' アーム先端　ネジ吸着バルブ
227 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
228 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
229 '
230 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
231 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
232 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
233 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
234 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
235 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
236 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
237 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
238 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
239 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
240 '
241 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
242 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
243 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
244 '
245 Def Inte MOUT_LED1          ' 画像処理用LED照明
246 '
247 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
248 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
249 '
250 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
251 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
252 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
253 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
254 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
255 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
256 '
257 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
258 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
259 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
260 '
261 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
262 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
263 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
264 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
265 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
266 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
267 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
268 Y6A_VV1%    =  12250    ' アーム先端　ネジ吸着バルブ
269 Y6B_VB1%    =  12251    'アーム先端　吸着破壊バルブ
270 MOUT_VB1%   =  12251    ' アーム先端　ネジ吸着破壊バルブ
271 '
272 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
273 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
274 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
275 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
276 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
277 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
278 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
279 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
280 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
281 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
282 '
283 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
284 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
285 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
286 '
287 MOUT_LED1%  =  12239    ' 画像処理用LED照明
288 '
289 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
290 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
291 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
292 '
293 '共通
294 Def Inte MTEST_KEY                      'デバックテスト用
295 Def Inte MOn                            '出力=1
296 Def Inte MOff                           '出力=0
297 '
298 'ねじ締め装置_出力アドレス
299 Def Inte MOUT_ScwT_ComChk               '通信確認
300 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
301 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
302 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
303 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
304 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
305 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
306 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
307 'ねじ締め装置_入力アドレス
308 Def Inte MIN_ScwT_comOK                 '通信確認返信
309 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
310 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
311 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
312 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
313 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
314 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
315 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
316 '
317 Dim MScwT_Case1%(2)               '条件1停止変数
318 Dim MScwT_Case2%(2)               '条件2停止変数
319 Dim MScwT_Case3%(2)               '条件3停止変数
320 Dim MScwT_Case4%(2)               '条件4停止変数
321 Dim MScwT_Case5%(2)               '条件5停止変数
322 '
323 '共通
324 MTEST_KEY% = 11359                       'デバッグ用テストKEY
325 MOn% = 1                                 '出力 = 1
326 MOff% = 0                                '出力 = 0
327 '
328 'ねじ締め機_アドレス設定
329 MOUT_ScwT_ComChk% = 12832               '通信確認送信
330 MOUT_ScwT_ST% = 12865                   'ねじ締め開始を送信
331 MOUT_ScwT_ReSTOK% = 12866               '再開始受信を送信
332 MOUT_ScwT_FinOK% = 12868                'ねじ締め完了受信を送信
333 MOUT_ScwT_Case1OK% = 12874              '条件1停止受信を送信
334 MOUT_ScwT_Case2OK% = 12875              '条件2停止受信を送信
335 MOUT_ScwT_Case3OK% = 12876              '条件3停止受信を送信
336 MOUT_ScwT_Case4OK% = 12877              '条件4停止受信を送信
337 MOUT_ScwT_Case5OK% = 12878              '条件5停止受信を送信
338 '
339 MIN_ScwT_comOK% = 11840                 'ねじ締め装置から返信
340 MIN_ScwT_STRec% = 11873                 'ねじ締め開始を受信
341 MIN_ScwT_ReST% = 11874                  '再開始を受信
342 MIN_ScwT_Fin% = 11876                   'ねじ締め完了を受信
343 MIN_ScwT_Case1% = 11882                 '条件1停止待機を受信
344 MIN_ScwT_Case2% = 11883                 '条件2停止待機を受信
345 MIN_ScwT_Case3% = 11884                 '条件3停止待機を受信
346 MIN_ScwT_Case4% = 11885                 '条件4停止待機を受信
347 MIN_ScwT_Case5% = 11886                 '条件5停止待機を受信
348 '
349 MScwT_Case1%(1) = MIN_ScwT_Case1%
350 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
351 MScwT_Case2%(1) = MIN_ScwT_Case2%
352 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
353 MScwT_Case3%(1) = MIN_ScwT_Case3%
354 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
355 MScwT_Case4%(1) = MIN_ScwT_Case4%
356 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
357 MScwT_Case5%(1) = MIN_ScwT_Case5%
358 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
359 '
360 '設定 InitialZoneBで使用する変数
361 Def Pos PActive       '直交座標系 位置変数 現在位置
362 Def Pos Pmove         '直交座標系 位置変数 移動先
363 Def Jnt JActive       '関節座標系 位置変数 現在位置
364 Def Jnt Jmove         '関節座標系 位置変数 移動先
365 Def Jnt JTaihi        '関節座標系 位置変数 退避ポジション ティーチングで設定
366 Def Inte MRecoveryPass      '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行
367 Def Inte MJ6          'J6軸の値を比較する為の変数
368 Def Inte MStandby              '待機位置確認フラグ
369 Def Inte MRecoveryChuckOpen    'チャック解放フラグ（復帰動作前）両掴み対策
370 '★注意★初期位置を変更した時には、変更が必要！
371 '
372 '
373 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
374 Function M% fnAssyStart
375     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
376 '   BaseUnit6通信確認 CDに伴い削除 2022/07/27 M.H
377 '
378 '
379 ' PIASチケット読込み工程抜け確認
380     M_20# = MClear%                       '初期化
381 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
382 '        MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
383 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
384 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
385 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' メニューへ戻る
386 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NGを工程履歴に書込み次の工程へ
387 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NGを工程履歴に書込み次の工程へ
388 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' 履歴NG, 工程抜け
389 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK以外は組立終了
390 '    EndIf
391 '    '
392 '    '座標移動
393 '    '
394 '    '条件xx停止
395 '    fScewTCaseStop(MScwT_Case5%)
396 '    '
397 '    'ベースユニットKEY
398 '    Wait M_In(MTEST_KEY%) = MOn%
399 '    '
400 '    '再開始
401 '    fScewTReStart()
402 '    '
403 '    '座標移動
404 '    '
405 '    'ねじ締め完了
406 '    Mret% = fScewTFinish()
407 ' ネジ締めテスト終了
408 ' PIASテスト -----------
409 '    MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
410 '    MRet% = fnPiasWrite(MNG%)
411  '   MRet% = fnPCBNumberCheck()
412 ' PIASテスト終了 -------
413 '
414     '組立開始(9/6追加(中村))
415     'プログラム原点
416     Ovrd 100
417     ' ハンド状態初期化(10/29追加M.H)(2/11修正(中村))
418     Cmp Off                     'コンプライアンスモード終了
419     ColChk On                   '衝突検知ON
420     If M_In(11266) Then
421         M_Out(12256) = 0
422         M_Out(12257) = 1
423     EndIf
424     If M_In(11269) Then
425         M_Out(12258) = 0
426         M_Out(12259) = 1
427     EndIf
428     If M_In(11271) Then
429         M_Out(12260) = 0
430         M_Out(12261) = 1
431     EndIf
432     *WAIT_HAND_INI
433     If M_In(11265) = 1 And M_In(11268) = 1 And M_In(11270) = 1 Then GoTo *CompHandIni Else GoTo *WAIT_HAND_INI
434     *CompHandIni
435     M_Out(12257) = 0
436     M_Out(12259) = 0
437     M_Out(12261) = 0
438 '
439 '
440 'Dly 5                           'デバッグ用(22/09/30中村)
441     ' ねじ締め機テスト用 ----------
442     Mret% = fScrewTcomChk()
443     If Mret% = -1 Then GoTo *ASSY_ERROR_END
444     'チケットIDを読む
445     M_Out(12262) = 1 Dly 0.5 '本体位置決め出端ON
446     PTemp = P_Curr
447     MRtn = 0
448 '    If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
449 '        If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
450 '            If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
451 '                MRtn = 1
452 '            EndIf
453 '        EndIf
454 '    EndIf
455 '    If MRtn = 1 Then
456 '        Mov PTicketRead
457 '    Else
458 '        Cnt 1 , 10 , 10
459 '        Mov PInitialPosition
460 '        Mov PTicketRead_1           'チケットID読み取り回避点
461 '        Cnt 0
462 '        Mvs PTicketRead             'ID読み位置
463 '    EndIf
464 '
465 ' 2022/04/12 安全方向へ条件変更 渡辺
466 ' PInitialPosition 在席 MStandby=2
467 ' PTicketRead_1 在席 MStandby=1
468 '
469     MStandby = 0    '待機位置フラグを初期化
470     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
471         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
472             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
473                 MStandby = 2
474             EndIf
475         EndIf
476     EndIf
477     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
478         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
479             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
480                 MStandby = 1
481             EndIf
482         EndIf
483     EndIf
484     If MStandby = 2 Then
485         Mov PTicketRead_1           'チケットID読み取り回避点
486         Cnt 0
487     EndIf
488     If MStandby <> 0 Then GoTo *PositionOK
489     fErrorProcess(11,230,281,0)            '初期位置にいない時はエラーにする
490     If M_20# = MNext% Then GoTo *ASSY_ERROR_END
491     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
492     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
493     If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
494     *PositionOK
495 '
496     Mvs PTicketRead             'ID読み位置
497 ' CDに伴い削除 2022/07/27 M.H
498 '    M_Out(12259) = 0            'DVDメカチャック開OfF
499 '    M_Out(12258) = 1            'DVDメカチャック閉ON
500 '
501     '
502     MRtn = 1        'MRtn初期化
503 *RE_TICKET_READ
504 '    MRtn = fnPiasCheck()               'ID読み取り
505 'PInspPosition(1) = PTicketRead  'IDチケット読取位置
506 'MInspGroup%(1) = 1              '検査G番号
507 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
508 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
509     MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
510     '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
511     '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
512 EndIf
513 If MRtn = 1 Then GoTo *CompRead
514 '
515     'エラー時製品位置決めを解除
516 *RE_ERR_REL_1
517 If M_20# = MContinue% Then M_20# = MRtn
518 M_Out(12263) = 1 Dly 0.5    '製品位置決めを解除
519 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)
520 '
521 If MRtn = 1 Then GoTo *CompErrorRelease
522 MRtn = M_20#        'M_20#一時避難
523 M_20# = MClear%
524 fErrorProcess(11,234,284,0)     '位置決め戻端エラー
525 If M_20# = MContinue% Then GoTo *RE_ERR_REL_1
526 If M_20# = MNext% Then M_20# = MRtn
527 If M_20# = MNgProcess% Then M_20# = MAbout%
528 *CompErrorRelease
529 '
530 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
531 If M_20# = MNext% Then M_20# = MPass%
532 Mvs PTicketRead_1                         '22/04/07 追加 渡辺
533 GoTo *ASSY_ERROR_END
534 *CompRead
535     fScrewTStart()           '処理位置変更2/27中村)
536 '
537     'パレットから製品を取る
538     '
539     *RE_POSITIONING
540     '
541     M_Out(12262) = 1 Dly 0.5 '本体位置決め出端ON
542 '    Wait M_In(11273) = 1     '本体位置決め出端検出
543     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '本体位置決め出端検出
544     If MRtn = 1 Then GoTo *CompPositioning
545     fErrorProcess(11,231,282,0)
546     If M_20# = MNext% Then M_20# = MClear%
547     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
548     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
549     If M_20# = MContinue% Then GoTo *RE_POSITIONING
550     *CompPositioning
551 '
552     Mov PProductOnPltGet_2      '本体受け取り上空回避点
553 '
554 ''    Wait M_In(11888) = 1        'ねじロボ2停止1受信(処理位置変更3/14中村)
555 '    MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
556 '    *RE_ERR_REL_2
557 '    If M_20# = MContinue% Then M_20# = MRtn2
558 '    If MRtn = 0 Then
559 '        MRtn2 = 1       'MRtn2初期化
560 '        M_Out(12263) = 1 Dly 0.5    '製品位置決めを解除
561 '        Mov PInitialPosition  '"イニシャルに戻る動き"
562 '        MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
563 '        If MRtn2 = 0 Then
564 '            MRtn2 = M_20#                   '位置決め戻端エラーならM_20#内部を一時避難
565 '            M_20# = MClear%                 'M_20#初期化
566 '            fErrorProcess(11,234,284,0)     '位置決め戻端エラー
567 '            If M_20# = MNext% Then M_20# = MRtn2        'M_20#に避難した値を代入
568 '                '位置決めエラーが発生して工程を抜ける場合停止処理を行う
569 '            If M_20# = MNgProcess% Then M_20# = MAbout%
570 '            Break
571 '        EndIf
572 '        Break
573 '            EndIf
574 '    If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
575 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
576 '
577 '    Mov PProductOnPltGet_1      '本体受け取り上空
578     '
579     *RE_PLT_GET_1
580     '
581     M_Out(12256) = 0            '本体チャック閉OFF
582     M_Out(12257) = 1            '本体チャック開ON
583     '
584 '    Wait M_In(11265) = 1        '本体チャック開検出
585     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
586     If MRtn = 1 Then GoTo *CompPltGet1
587     fErrorProcess(11,244,284,0)
588     If M_20# = MNext% Then M_20# = MClear%
589     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
590     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
591     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
592     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
593     *CompPltGet1
594     '
595     Mov PProductOnPltGet_1      '本体受け取り上空
596     '
597     Ovrd 25
598 '    Fine 0.05 , P
599     Mvs PProductOnPltGet        '本体受け取り位置
600     Dly 0.1
601     M_Out(12257) = 0            '本体チャック開OFF
602     M_Out(12256) = 1            '本体チャック閉ON
603 '    Fine 0 , P
604     '
605     M_Out(12263) = 1 Dly 0.5                    '本体位置決め戻端ON
606 '    Wait M_In(11274) = 1     '本体位置決め戻端検出
607     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)    '本体位置決め戻端検出
608     If MRtn = 1 Then GoTo *CompPltGet2
609     M_Out(12256) = 0                            '本体チャック閉OFF
610     M_Out(12257) = 1                            '本体チャック開ON
611     Dly 2.0
612     Mvs PProductOnPltGet_1
613     Mov PProductOnPltGet_2
614     fErrorProcess(11,234,284,0)
615     If M_20# = MNext% Then M_20# = MClear%
616     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
617     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
618     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
619     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
620     Mov PProductOnPltGet_1
621     Mvs PProductOnPltGet
622     M_Out(12257) = 0                            '本体チャック開OFF
623     M_Out(12256) = 1                            '本体チャック閉ON
624     Dly 2.0
625     *CompPltGet2
626     '
627 '    Wait M_In(11264) = 1        '本体検出
628     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '本体検出
629     If MRtn = 1 Then GoTo *CompPltGet3
630     M_Out(12256) = 0            '本体チャック閉OFF
631     M_Out(12257) = 1            '本体チャック開ON
632     Dly 2.0
633     Mvs PProductOnPltGet_1
634     Mov PProductOnPltGet_2
635     fErrorProcess(11,252,284,0)
636     If M_20# = MNext% Then M_20# = MClear%
637     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
638     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
639     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
640     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
641     Mov PProductOnPltGet_1
642     Mvs PProductOnPltGet
643     M_Out(12257) = 0            '本体チャック開OFF
644     M_Out(12256) = 1            '本体チャック閉ON
645     Dly 2.0
646     *CompPltGet3
647     '
648 '    Wait M_In(11266) = 1        '本体チャック閉検出
649     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
650     If MRtn = 1 Then GoTo *CompPltGet4
651     M_Out(12256) = 0            '本体チャック閉OFF
652     M_Out(12257) = 1            '本体チャック開ON
653     Dly 2.0
654     Mvs PProductOnPltGet_1
655     Mov PProductOnPltGet_2
656     Dly 0.1
657     M_Out(12257) = 0            '本体チャック開OFF
658     M_Out(12256) = 1            '本体チャック閉ON
659     Dly 3.0
660     fErrorProcess(11,245,284,0)
661     If M_20# = MNext% Then M_20# = MClear%
662     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
663     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
664     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
665     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
666     M_Out(12256) = 0            '本体チャック閉OFF
667     M_Out(12257) = 1            '本体チャック開ON
668     Dly 2.0
669     Mov PProductOnPltGet_1
670     Mvs PProductOnPltGet
671     M_Out(12257) = 0            '本体チャック開OFF
672     M_Out(12256) = 1            '本体チャック閉ON
673     Dly 2.0
674     *CompPltGet4
675     '
676     Dly 0.1                     '念のためディレイ
677     Cnt 1 , 100 , 100
678     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
679     Mvs PProductOnPltGet_1      '本体受け取り上空
680     MRtn = FnCtlValue2(99)       '読書開始信号OFF  2022/04/28 渡辺
681     Ovrd 100
682     Accel 50 , 50
683     Mov PProductOnPltGet_2      '本体受け取り上空回避点
684     '
685     '製品をねじロボ2に置く
686     Mov PProductOnRoboSet_3     '経路
687     Accel 100 , 100
688     Cnt 0
689 '    Wait M_In(11888) = 1        'ねじロボ2停止1受信(処理位置,処理変更3/1中村)
690     MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
691     *RE_ERR_REL_2
692     If MRtn = 0 Then
693         Cnt 0
694         Mov PProductOnPltSet_2
695         Mov PProductOnPltSet_1
696         Mvs PProductOnPltSet
697         M_Out(12256) = 0        '本体チャック閉OFF
698         M_Out(12257) = 1        '本体チャック開ON
699         Dly 2.0
700         Mvs PProductOnPltSet_1
701         Mvs PProductOnPltSet_2
702         Mov PInitialPosition
703     EndIf
704     If MRtn = 0 Then GoTo *ASSY_ERROR_END
705     '
706     *RE_ROBO_SET_1
707     '
708     M_Out(12259) = 0            'DVDメカチャック開OfF
709     M_Out(12258) = 1            'DVDメカチャック閉ON
710 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
711     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
712     If MRtn = 1 Then GoTo *CompRoboSet1
713     fErrorProcess(11,269,284,0)
714     If M_20# = MNext% Then M_20# = MClear%
715     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
716     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
717     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
718     *CompRoboSet1
719 '
720     Mvs PProductOnRoboSet_2     'ねじロボ製品置き上空回避点
721 '    Mvs PProductOnRoboSet_4     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)
722     Ovrd 25
723     Mvs PProductOnRoboSet_1     'ねじロボ製品置き上空
724     Mvs PProductOnRoboSet       'ねじロボ製品置き位置
725     M_Out(12866) = 1 Dly 0.3    'ねじロボ2動作再開(停止1～停止2)
726 '    Wait M_In(11889) = 1        'ねじロボ2停止2受信
727     MScrewRoboNgFlg% = 0
728     MRtn = fScrewTighenRoboCheck(11889)    '停止状態を受信する
729     If MRtn = 0 Then
730         MScrewRoboNgFlg% = 1
731     EndIf
732 '
733     *RE_ROBO_SET_2
734 '
735     M_Out(12256) = 0            '本体チャック閉OFF
736     M_Out(12257) = 1            '本体チャック開ON
737 '    Wait M_In(11265)            '本体チャック開検出
738     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
739     If MRtn = 1 Then GoTo *CompRoboSet2
740     fErrorProcess(11,244,284,0)
741     If M_20# = MNext% Then M_20# = MClear%
742     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
743     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
744     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_2
745     *CompRoboSet2
746     '
747     Mvs PProductOnRoboSet_1     'ねじロボ製品置き上空
748     Mvs PProductOnRoboSet_2     'ねじロボ製品置き上空回避点
749 '    Mvs PProductOnRoboSet_4     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)
750     Ovrd 100
751     Cnt 1 , 10 , 10
752     Mov PProductOnRoboSet_3     '経路
753     '
754     If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition
755     If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
756 '
757 '
758 '
759     '
760     'チルトスライダーを押す
761     Cnt 1 , 10
762     Mov PPushTilt_3             'チルトスライダー押し向き変え回避点
763     Cnt 0
764     Mov PPushTilt_2             'チルトスライダー押し回避点
765     Ovrd 30
766     Mvs PPushTilt_1             'チルトスライダー押し上空
767     Spd 1000
768     Ovrd 5
769     Mvs PPushTilt               'チルトスライダー押し
770     Spd M_NSpd
771     Ovrd 50
772     Cnt 1 , 1 , 1
773     Mvs PPushTilt_1             'チルトスライダー押し上空
774     Cnt 1 , 10 , 10
775     Ovrd 100
776     Mov PPushTilt_2             'チルトスライダー押し回避点
777     Cnt 1 , 100 , 100
778     '
779     '背面板を取る(コンプライアンスモード実装11/8中村)
780     Mov PPlateBackGet_2         '背面板受け取り上空回避点
781 '    Cnt 1 , 10'暫定
782     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止2～停止3)
783 '    MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する'12/20位置の変更(中村)
784 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
785 '    Mov PPlateBackGet_1         '背面板受け取り上空'暫定
786     Cnt 0
787     Mov PPlateBackGet_1         '背面板受け取り上空
788     '
789     *RE_PLATE_GET
790     '
791     Fine 0.05 , P               'ファイン命令ON
792     Ovrd 25
793     Mvs PPlateBackGet           '背面板受け取り位置
794 '    Dly 0.2                     '一時コメントアウト
795     M_Out(12257) = 0            '本体チャック開OFF
796     M_Out(12256) = 1            '本体チャック閉ON
797     '
798 '    Wait M_In(11266) = 1        '本体チャック閉検出
799     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
800     If MRtn = 1 Then GoTo *CompPlateGet_1
801     M_Out(12256) = 0            '本体チャック閉OFF
802     M_Out(12257) = 1            '本体チャック開ON
803     Mvs PPlateBackGet_1
804     fErrorProcess(11,245,293,0) '284→293に変更6/2中村
805     If M_20# = MNext% Then M_20# = MClear%
806     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
807     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
808     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
809     Mvs PPlateBackGet           '背面板受け取り位置
810     M_Out(12257) = 0            '本体チャック開OFF
811     M_Out(12256) = 1            '本体チャック閉ON
812     *CompPlateGet_1
813     Fine 0 , P                  'ファイン命令OFF
814     '
815     Ovrd 5
816     Accel 25 , 100
817     Dly 0.7                     'ディレイ時間調節中(把持力確保)
818 '    CmpG 0.7,0.7,,,,,,       'X,Y軸ゲインを0.7に変更
819 '    ColChk Off                  '衝突検知OFF
820 '    Cmp Pos , &B11          'X,Y軸コンプライアンスモード開始
821     Mov PPlateBackGet_1         '背面板受け取り上空
822     Cnt 1 , 10 , 10
823 '    Cmp Off                     'コンプライアンスモード終了
824 '    ColChk On                   '衝突検知ON
825     Ovrd 50
826     Mov PPlateBackGet_2         '背面板受け取り上空回避点
827     Ovrd 100
828     Accel 100 , 100
829     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '背面パネルチェック
830     If MRtn = 1 Then GoTo *CompPlateGet_2
831     Cnt 0
832     fErrorProcess(11,267,293,0)                     '284→293に変更6/2中村
833     If M_20# = MNext% Then M_20# = MClear%
834     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
835     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
836     If M_20# = MContinue% Then
837         Mov PPlateBackGet_1
838         Dly 0.3
839         M_Out(12256) = 0            '本体チャック閉OFF
840         M_Out(12257) = 1            '本体チャック開ON
841         Dly 2.0
842     EndIf
843     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
844     *CompPlateGet_2    '
845     '背面板を置く
846 '    Wait M_In(11889) = 1        'ねじロボ2停止2受信
847     ColChk Off
848     Cnt 1 , 100 , 100           '100mm近傍追加(221221中村)
849     Mov PPlateBackSet_13        '背面板置き上空
850     Cnt 1 , 10 , 10
851 '
852     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        'もう一度背面パネルチェック
853     If MRtn = 1 Then GoTo *CompPlateGet_3
854     fErrorProcess(11,267,293,0)                     '284→293に変更6/2中村
855     If M_20# = MNext% Then M_20# = MClear%
856     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
857     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
858     If M_20# = MContinue% Then
859         Mov PPlateBackGet_2
860         Mov PPlateBackGet_1
861         M_Out(12256) = 0            '本体チャック閉OFF
862         M_Out(12257) = 1            '本体チャック開ON
863         Dly 2.0
864     EndIf
865     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
866     *CompPlateGet_3
867 '
868 ' CDに伴い削除 2022/07/27 M.H
869 '    ' 部品供給要求送信
870 '    M_Out(12787) = 1
871 '
872     MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する'12/20位置の変更(中村)
873 '    If MRtn = 0 Then Mov PInitialPosition    '"イニシャルに戻る動き"
874     If MRtn = 0 Then GoTo *ASSY_ERROR_END
875 '
876     Mov PPlateBackSet_12        '爪入れ前回避点
877     Cnt 0
878     Ovrd 25
879     Accel 25 , 25
880     Mvs PPlateBackSet_11        '爪入れ込み前
881     Mvs PPlateBackSet_10        '爪入れ込み1
882     Mvs PPlateBackSet_9         '爪入れ込み2
883 '    CmpG 1.0,1.0,1.0,1.0,1.0,1.0,,
884 '    Cmp Pos, &B001000
885     Cnt 1                       '減速で次の移動へ(221221中村)
886     Mov PPlateBackSet_8         '経路1
887     Mov PPlateBackSet_7         '経路2
888     Mov PPlateBackSet_6         '経路3
889     Mov PPlateBackSet_5         '経路4
890     Mov PPlateBackSet_4         '経路
891     Mov PPlateBackSet_3         '経路6
892     Mov PPlateBackSet_2         '経路7
893     Mov PPlateBackSet_1         '経路8
894     Mov PPlateBackSet           '背面板離し位置
895 '    Cmp Off
896     Accel 100 , 100
897     Cnt 0
898     Dly 0.1
899 '
900     *RE_PLATE_SET
901     M_Out(12256) = 0            '本体チャック閉OFF
902     M_Out(12257) = 1            '本体チャック開ON
903     '
904 '    Wait M_In(11265)            '本体チャック開検出
905     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
906     If MRtn = 1 Then GoTo *CompPlateSet
907     fErrorProcess(11,244,284,0)
908     If M_20# = MNext% Then M_20# = MClear%
909     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
910     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
911     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
912     *CompPlateSet
913     '
914 '
915 '-----暫定押し-------------------------------------(22/12/21中村)ここから
916 *RE_BUCK_PUSH
917     M_20# = MClear%
918     Mov PPlateBackPush_2
919 '
920     M_Out(12257) = 0            '本体チャック開OFF
921     M_Out(12256) = 1            '本体チャック閉ON
922 '
923     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
924 '
925     If MRtn = 1 Then GoTo *CompBuckPushSetting  '正常なら押し動作へ
926 '
927     fErrorProcess(11,245,287,0) '閉端センサーNG時エラー表示
928         If M_20# = MNext% Then M_20# = MClear%              '次へが押されたら値を初期化
929         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '停止が押されたらエラーエンドへ
930         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END    'NGが押されたらエラーエンドへ
931         If M_20# = MContinue% Then GoTo *RE_BUCK_PUSH       'リトライが押されたらもう一度閉じる
932 '
933 *CompBuckPushSetting
934 '
935     Mvs PPlateBackPush_1
936     Ovrd 10
937     Mvs PPlateBackPush
938 '    Dly 0.1
939 '背面クランプここから(12/15)
940     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～停止4)
941     MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
942         If MRtn = 0 Then
943             Mvs PPlateBackPush_1
944             Mov PPlateBackSet_12
945             Mov PInitialPosition    '"イニシャルに戻る動き"
946         EndIf
947         If MRtn = 0 Then GoTo *ASSY_ERROR_END
948 '背面クランプここまで(12/15)
949     Ovrd 50
950     Mvs PPlateBackPush_1
951 *RE_CHUCK_OPEN
952     M_20# = MClear%
953     M_Out(12256) = 0            '本体チャック閉OFF
954     M_Out(12257) = 1            '本体チャック開ON
955     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
956     If MRtn = 1 Then GoTo *CompChuckOpenForBackPush
957     fErrorProcess(11,244,284,0)
958         If M_20# = MNext% Then M_20# = MClear%              '次へが押されたら値を初期化
959         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '停止が押されたらエラーエンドへ
960         If M_20# = MNgProcess% Then GoTo *RE_CHUCK_OPEN    'NGが押されたらエラーエンドへ
961         If M_20# = MContinue% Then GoTo *RE_CHUCK_OPEN       'リトライが押されたらもう一度閉じる
962 *CompChuckOpenForBackPush
963 '-----暫定押し-------------------------------------(22/12/21中村)ここまで
964     '
965     ColChk On
966     Mov PPlateBackSet_13        '背面板置き上空
967 '    M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～停止4)
968     Ovrd 100
969     '
970 ''    ' 部品供給要求送信(処理位置変更2/27中村)
971 '    M_Out(12787) = 1
972     'ねじロボ製品クランプ固定待ち
973 '    Wait M_In(11891) = 1        'ねじロボ2停止4受信
974 'MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
975 'If MRtn = 0 Then Mov PInitialPosition    '"イニシャルに戻る動き"
976 'If MRtn = 0 Then GoTo *ASSY_ERROR_END
977     '置き位置画像検査
978 '    Mov PPlateBackCheck_3       '画像検査位置上空
979 '    Mov PPlateBackCheck_2       '通過点
980 '    Mvs PPlateBackCheck         '確認位置
981     '
982     'PInspPosition(2) = PPlateBackCheck
983     'MInspGroup(2) = 2
984     'MRtn = ISInspection(PInspPosition,MInspGroup,2,-1,1)
985     'If MRtn <> 1 Then
986     '   'エラー処理
987     'EndIf
988     '
989 ''    ' 部品供給要求送信
990 '    M_Out(12787) = 1    '処理位置変更
991 '    Mov PPlateBackCheck_3       '画像検査位置上空
992     '
993     'ねじロボ引き込み待ち
994     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～完了)
995     '
996     'DVDメカを取る CDに伴い削除 2022/07/27 M.H
997 *Loop_CW_CCW_S
998     *RE_MECHA_SET_1
999 MRtn = fScrewTighenRoboCheck(11876)    '停止状態を受信する
1000 'If MRtn = 0 Then Mov PInitialPosition   '"イニシャルに戻る動き"
1001 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1002 '
1003     *CompRoboGet1
1004     '
1005 '    Ovrd 50
1006     Mov PProductOnRoboGet_3     'ねじロボ製品取り上空回避点2から3へ
1007 '    Ovrd 20
1008     Mvs PProductOnRoboGet_2     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から2へ
1009     Ovrd 20
1010     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
1011     Ovrd 10
1012     Mvs PProductOnRoboGet       'ねじロボ製品取り位置
1013     Dly 0.1
1014 '
1015     *RE_ROBO_GET_2
1016 '
1017     M_Out(12257) = 0            '本体チャック開OFF
1018     M_Out(12256) = 1            '本体チャック閉ON
1019 '
1020 '    Wait M_In(11266) = 1        '本体チャック閉検出
1021     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
1022     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
1023     M_Out(12256) = 0            '本体チャック閉OFF
1024     M_Out(12257) = 1            '本体チャック開ON
1025     Dly 2.0
1026     Mvs PProductOnRoboGet_1
1027     Mvs PProductOnRoboGet_2
1028     Mov PProductOnRoboGet_3
1029     Mov PProductOnRoboGet_4
1030     Mov PInitialPosition
1031     M_Out(12257) = 0            '本体チャック開OFF
1032     M_Out(12256) = 1            '本体チャック閉ON
1033     Dly 1.0
1034     fErrorProcess(11,245,284,0)
1035     If M_20# = MNext% Then MRtn = 1
1036     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1037     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1038     M_Out(12256) = 0            '本体チャック閉OFF
1039     M_Out(12257) = 1            '本体チャック開ON
1040     Dly 2.0
1041     Mov PProductOnRoboGet_4
1042     Mov PProductOnRoboGet_3
1043     Mov PProductOnRoboGet_2
1044     Mvs PProductOnRoboGet_1
1045     Mvs PProductOnRoboGet
1046     If M_20# = MContinue% Or M_20# = MNext% Then GoTo *RE_ROBO_GET_2
1047     *CompRoboGet2
1048     M_20# = MClear%
1049     '
1050     Dly 0.1
1051     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
1052     Ovrd 50
1053     Mvs PProductOnRoboGet_2     'ねじロボ製品取り上空回避点(9/27暫定コメントアウト)12/15コメント解除
1054     Mvs PProductOnRoboGet_3     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から3へ
1055     Ovrd 100
1056     Mov PProductOnRoboGet_4     '経路3から4へ
1057     Cnt 1 , 100 , 100
1058 '
1059     M_Out(12868) = 1 Dly 0.3    'ねじロボ2動作完了を送信
1060     *RE_ROBO_GET_3
1061 ' CDに伴い削除 2022/07/27 M.H
1062 '    M_Out(12258) = 0            'DVDメカチャック閉OFF
1063 '    M_Out(12259) = 1            'DVDメカチャック開ON
1064 ''    Wait M_In(11268) = 1        'DVDメカチャック開検出
1065 '    MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1066 '    If MRtn = 1 Then GoTo *CompRoboGet3
1067 '    fErrorProcess(11,270,284,0)
1068 '    If M_20# = MNext% Then M_20# = MClear%
1069 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1070 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1071 '    If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1072     *CompRoboGet3
1073     '
1074     'パレットへ製品を置く
1075     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1076     Cnt 1 , 10
1077     Mov PProductOnPltSet_1      '本体置き位置上空
1078     Cnt 0
1079     Ovrd 10
1080     Mvs PProductOnPltSet        '本体置き位置
1081     Dly 0.1
1082 '
1083     *RE_PLT_SET
1084 '
1085     M_Out(12256) = 0            '本体チャック閉OFF
1086     M_Out(12257) = 1            '本体チャック開ON
1087 '
1088     Wait M_In(11265) = 1        '本体チャック開検出
1089 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
1090     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
1091     If MRtn = 1 Then GoTo *CompPltSet
1092     fErrorProcess(11,244,284,0)
1093     If M_20# = MNext% Then M_20# = MClear%
1094     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1095         Mvs PProductOnPltSet_1
1096         Mov PProductOnPltSet_2
1097         Mov PInitialPosition
1098     EndIf
1099     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1100     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1101     If M_20# = MContinue% Then GoTo *RE_PLT_SET
1102     *CompPltSet
1103 '
1104     Mvs PProductOnPltSet_1      '本体置き位置上空
1105     Ovrd 100
1106     Cnt 1 , 10 , 10
1107     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1108 '    Mov PInitialPosition        'イニシャルポジション
1109     MRtn = FnCtlValue2(2)          '組立ＯＫ＋１  2022/04/28 渡辺
1110     Mov PTicketRead_1           'チケットID読み取り回避点
1111     Cnt 0
1112     MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1113     '
1114     'チケットID書き込み
1115     M_20# = MAssyOK%
1116     *ASSY_ERROR_END
1117     *AssyEnd
1118     *fnAssyStart_FEndPosi
1119 FEnd
1120 '
1121 '■fnPiasCheck
1122 ''' <summary>
1123 ''' PIASチケット読込み
1124 ''' </summary>
1125 ''' <returns>   0 : NG
1126 '''             1 : OK(読込み完了)
1127 ''' </returns>
1128 ''' <remarks>
1129 ''' Date   : 2021/07/07 : M.Hayakawa
1130 ''' </remarks>'
1131 Function M% fnPiasCheck
1132     fnPiasCheck = 0
1133     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1134     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1135 '
1136 *RETRY_PIAS
1137     M_20# = MClear%
1138     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1139     '
1140     '【IDチケット読み込み】
1141     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1142     MInspGroup%(1) = 1              '検査G番号
1143     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1144 '
1145     'エラーの場合
1146     If MRtn <> 1 Then
1147         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1148         If MRtn <> 1 Then
1149             'D720 -> D1300 コピー要求
1150             M_Out(12565) = 1
1151             Dly 0.5
1152             M_Out(12565) = 0
1153             'エラー処理記述
1154             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1155             'GOT KEY入力待ち
1156             MKeyNumber = fnKEY_WAIT()
1157             '
1158             Select MKeyNumber
1159                 Case MNext%         '次へを選択した場合
1160                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1161                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1162                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1163                     Break
1164                 Case MAbout%        '停止を選択した場合
1165                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1166                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1167                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1168                     Break
1169                 Case MNgProcess%    'NGを選択した場合
1170                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1171                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1172                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1173                     Break
1174                 Case MContinue%     '継続を選択した場合
1175                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1176                     M_20# = MContinue%
1177                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1178                     Break
1179             End Select
1180         EndIf
1181     EndIf
1182 '----------D720 -> D1300 コピー要求----------
1183     M_Out(12565) = 1
1184     Dly 0.5
1185     M_Out(12565) = 0
1186 '----------通信確認をする----------
1187     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1188     MRtn = 0                ' 初期化
1189     M_20# = MClear%         ' 初期化
1190     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1191     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1192     If MRtn <> 1 Then
1193         If M_20# = MContinue% Then
1194             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1195         Else
1196             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1197         EndIf
1198     EndIf
1199 '----------工程抜け確認----------
1200     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1201     MRtn = 0                ' 初期化
1202     M_20# = MClear%         ' 初期化
1203     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1204     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1205     If MRtn <> 1 Then
1206         If M_20# = MContinue% Then
1207             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1208         Else
1209             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1210         EndIf
1211     EndIf
1212     '
1213     fnPiasCheck = 1
1214     *fnPiasCheck_End
1215 FEnd
1216 '
1217 '■fnPCComuCheck
1218 ''' <summary>
1219 ''' PC-PLC通信チェック
1220 ''' </summary>
1221 ''' <returns>   0 : NG
1222 '''             1 : OK(読込み完了)
1223 ''' </returns>
1224 ''' <remarks>
1225 ''' Date   : 2021/07/07 : M.Hayakawa
1226 ''' </remarks>'
1227 Function M% fnPCComuCheck
1228     fnPCComuCheck = 0
1229     MJudge% = 0                                  '初期化
1230     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1231     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1232     '
1233     For MStaNo = 0 To 5
1234         '
1235         If M_In(MIN_PIAS_ComOK%) = 1 Then
1236             'PC通信OK(M400)
1237             MJudge% = MOK%
1238             MStaNo = 5
1239             Break
1240         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1241             'toRBT_通信確認time out
1242             MJudge% = MNG%
1243             MCommentD1001 = 15
1244             MCommentD1002 = 21
1245             MStaNo = 5
1246             Break
1247         Else
1248             'toRBT_通信確認time out
1249             MJudge% = MNG%
1250             MCommentD1001 = 14
1251             MCommentD1002 = 21
1252             Break
1253         EndIf
1254     Next MStaNo
1255     '
1256     '上記で返信フラグを受信してからPC通信確認OFF
1257     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1258     '
1259     'エラー画面
1260     If MJudge% <> MOK% Then
1261         M_20# = MClear%     '初期化
1262         'エラー処理記述
1263         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1264         'GOT KEY入力待ち
1265         MKeyNumber = fnKEY_WAIT()
1266         '
1267         If MKeyNumber = MAbout% Then            '停止を選択した場合
1268             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1269             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1270             Break
1271         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1272             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1273             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1274             Break
1275         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1276             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1277             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1278             Break
1279         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1280             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1281             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1282             Break
1283         EndIf
1284     Else
1285         'OKの場合
1286         fnPCComuCheck = 1
1287     EndIf
1288 FEnd
1289 '
1290 '■fnProcessCheck
1291 ''' <summary>
1292 ''' 工程抜け確認
1293 ''' </summary>
1294 ''' <returns>    1：工程履歴OK     0：異常終了
1295 '''             -1：前工程履歴NG  -2：自工程履歴あり
1296 '''             -3：モデル仕向NG  -4：タイムアウト
1297 '''             -5：履歴処理エラー
1298 ''' </returns>
1299 ''' <remarks>
1300 ''' Date   : 2021/07/07 : M.Hayakawa
1301 ''' </remarks>'
1302 Function M% fnProcessCheck
1303     fnProcessCheck = 0
1304     MJudge% = MNG%      '一旦NGを初期化とする
1305 '----------工程抜け確認----------
1306     MCommentD1001 = 0   'コメント初期化
1307     For MStaNo = 0 To 5
1308         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1309         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1310         '
1311         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1312             MJudge% = MOK%
1313             fnAutoScreenComment(85)     ' AUTO画面
1314             MStaNo = 5
1315             Break
1316         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1317             MFlgLoop% = 0
1318             MJudge% = MNG%
1319             MCommentD1001 = 27
1320             MCommentD1002 = 22
1321             fnAutoScreenComment(94)     ' AUTO画面
1322             fnProcessCheck = -2         ' NGは-2を返す
1323             MStaNo = 5
1324             Break
1325         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1326            MJudge% = MNG%
1327             MCommentD1001 = 31
1328             MCommentD1002 = 22
1329             fnAutoScreenComment(83)     ' AUTO画面
1330             fnProcessCheck = -3         ' NGは-3を返す
1331             MStaNo = 5
1332             Break
1333         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1334             '履歴NGは直ぐに終了せず繰り返し確認を行う
1335             '前工程の書込みが終了していない可能性があるため
1336             MJudge% = MNG%
1337             MCommentD1001 = 32
1338             MCommentD1002 = 22
1339             fnAutoScreenComment(84)     ' AUTO画面
1340             fnProcessCheck = -1         ' NGは-1を返す
1341             Dly 1.0
1342             '工程抜け確認OFF
1343             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1344             Dly 1.0
1345            'MStaNo = 5
1346             Break
1347         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1348             MFlgLoop% = 0
1349             MJudge% = MNG%
1350             MCommentD1001 = 29
1351             MCommentD1002 = 22
1352             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1353             fnProcessCheck = -5         ' NGは-5を返す
1354             MStaNo = 5
1355             Break
1356         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1357             MJudge% = MNG%
1358             If MCommentD1001 = 32 Then
1359                 '何もしない
1360             Else
1361                 MCommentD1001 = 26
1362             EndIf
1363             MCommentD1002 = 22
1364             fnProcessCheck = -4         ' NGは-4を返す
1365             MStaNo = 5
1366             Break
1367         Else
1368             MJudge% = MNG%
1369             MCommentD1001 = 28
1370             MCommentD1002 = 22
1371         EndIf
1372     Next MStaNo
1373     '工程抜け確認OFF
1374     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1375     '通過履歴NG 工程抜けの場合
1376     If MJudge% = MPass% Then
1377         M_20# = MPass%
1378     EndIf
1379     '
1380     'エラー画面
1381     If MJudge% <> MOK% Then
1382         M_20# = MClear%     '初期化
1383         'エラー処理記述
1384         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1385         'GOT KEY入力待ち
1386         MKeyNumber = fnKEY_WAIT()
1387         '
1388         Select MKeyNumber
1389             Case MAbout%        '停止を選択した場合
1390                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1391                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1392                 Break
1393             Case MNext%         '次へを選択した場合
1394                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1395                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1396                 Break
1397             Case MContinue%     '継続を選択した場合
1398                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1399                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1400                 Break
1401             Case MNgProcess%    'NGを選択した場合
1402                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1403                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1404                 Break
1405         End Select
1406     Else
1407         fnProcessCheck = 1  ' OKは1を返す
1408     EndIf
1409 FEnd
1410 '
1411 '■fnPiasWrite
1412 ''' <summary>
1413 ''' Pias 組立結果書込み要求
1414 ''' </summary>
1415 '''<param name="MFlg%">
1416 '''                 MOK%(1) = 工程履歴にOKを書込む
1417 '''                 MNG%(0) = 工程履歴にNGを書込む
1418 '''</param>
1419 '''<returns></returns>
1420 ''' <remarks>
1421 ''' Date   : 2021/07/07 : M.Hayakawa
1422 ''' </remarks>'
1423 Function M% fnPiasWrite(ByVal MFlg%)
1424       fnPiasWrite = 0
1425 *RETRY_PIASWRITE
1426     '
1427     '組立OK(MOK%)の場合　M306 ON
1428    '組立NG(MNG%)の場合　M307 ON
1429     If MFlg% = MOK% Then
1430         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1431     Else
1432         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1433     EndIf
1434     Dly 0.1                  '念のため
1435     '
1436     'Piasへ書込み開始 M305 -> ON
1437     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1438     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1439     '
1440     MJudge% = MNG%
1441     '
1442     For MStaNo = 0 To 5
1443         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1444             MJudge% = MOK%
1445             'MRet = fnAutoScreenComment(85)  'AUTO画面
1446             MStaNo = 5
1447             Break
1448         '
1449         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1450             MJudge% = MNG%
1451             'MRet = fnAutoScreenComment(85)  'AUTO画面
1452            MCommentD1001 = 34
1453            MCommentD1002 = 25
1454             MStaNo = 5
1455             Break
1456         '
1457         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1458             MJudge% = MNG%
1459             'MRet = fnAutoScreenComment(85)  'AUTO画面
1460            MCommentD1001 = 35
1461            MCommentD1002 = 25
1462             MStaNo = 5
1463             Break
1464         '
1465         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1466             MJudge% = MNG%
1467             'MRet = fnAutoScreenComment(85)  'AUTO画面
1468            MCommentD1001 = 36
1469            MCommentD1002 = 25
1470             MStaNo = 5
1471             Break
1472         '
1473         Else
1474             MJudge% = MNG%
1475            MCommentD1001 = 42
1476            MCommentD1002 = 25
1477         '
1478         EndIf
1479         '
1480     Next MStaNo
1481     '
1482     'Piasへ書込み開始 M305 -> OfF
1483     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1484     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1485     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1486     '
1487     '
1488     '通過履歴NG 工程抜けの場合
1489     If MJudge% = MPass% Then
1490         M_20# = MPass%
1491     EndIf
1492     '
1493    M_20# = MClear%     '初期化
1494     '
1495     'エラー画面
1496     If MJudge% < MOK% Then
1497     '
1498 '残しておくが現状では使用しないラベル
1499 *RETRY_ERR_WRITE
1500         M_20# = MClear%     '初期化
1501         'エラー処理記述
1502         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1503         'GOT KEY入力待ち
1504         MKeyNumber = fnKEY_WAIT()
1505         '
1506         If MKeyNumber = MAbout% Then   '停止を選択した場合
1507             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1508            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1509             Break
1510         '
1511         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1512             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1513             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1514         '
1515         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1516             M_20# = MPass%            'M_20# プログラム間共通外部変数
1517             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1518         '
1519         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1520             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1521            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1522             Break
1523         '
1524         EndIf
1525         '
1526         If M_20# = MClear% Then *RETRY_ERR_WRITE
1527         '
1528     EndIf
1529     '
1530     If M_20# = MContinue% Then *RETRY_PIASWRITE
1531     '
1532     fnPiasWrite = 1
1533     '
1534 FEnd
1535 '
1536 '■fnPCBNumberCheck
1537 ''' <summary>
1538 ''' Pias 基板番号照合要求
1539 ''' </summary>
1540 '''<param name="%"></param>
1541 '''<param name="%"></param>
1542 '''<returns></returns>
1543 ''' <remarks>
1544 ''' Date   : 2021/07/07 : M.Hayakawa
1545 ''' </remarks>'
1546 Function M% fnPCBNumberCheck
1547       fnPCBNumberCheck = 0
1548     '
1549 *RETRY_PCBCHECK
1550     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1551     'Piasへ基板照合開始 M310 -> ON
1552     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1553     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1554     '
1555     MJudge% = MNG%
1556     '
1557     For MStaNo = 0 To 5
1558         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1559             MJudge% = MOK%
1560             fnAutoScreenComment(96)  'AUTO画面
1561             MStaNo = 5
1562             Break
1563         '
1564         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1565             MJudge% = MNG%
1566             fnAutoScreenComment(97)  'AUTO画面
1567             MCommentD1001 = 37
1568             MCommentD1002 = 25
1569             MStaNo = 5
1570             Break
1571         '
1572         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1573             MJudge% = MNG%
1574             fnAutoScreenComment(98)  'AUTO画面
1575             MCommentD1001 = 38
1576             MCommentD1002 = 25
1577             MStaNo = 5
1578             Break
1579         '
1580         ElseIf M_In(11580) = 1 Then                         'time out
1581             MJudge% = MNG%
1582             fnAutoScreenComment(99)  'AUTO画面
1583             MCommentD1001 = 39
1584             MCommentD1002 = 25
1585             MStaNo = 5
1586             Break
1587         '
1588         Else
1589             MJudge% = MNG%
1590            MCommentD1001 = 41
1591            MCommentD1002 = 25
1592         '
1593         EndIf
1594         '
1595     Next MStaNo
1596     '
1597     'Piasへ基板照合開始 M310 -> OfF
1598     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1599     '
1600     '
1601     '通過履歴NG 工程抜けの場合
1602     If MJudge% = MPass% Then
1603         M_20# = MPass%
1604     EndIf
1605     '
1606    M_20# = MClear%     '初期化
1607     '
1608     'エラー画面
1609     If MJudge% < MOK% Then
1610     '
1611 '残しておくが現状では使用しないラベル
1612 *RETRY_ERR_PCBNUMBER
1613         M_20# = MClear%     '初期化
1614         'エラー処理記述
1615         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1616         'GOT KEY入力待ち
1617         MKeyNumber = fnKEY_WAIT()
1618         '
1619         If MKeyNumber = MAbout% Then   '停止を選択した場合
1620             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1621             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1622             Break
1623         '
1624         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1625             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1626             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1627         '
1628         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1629             M_20# = MPass%            'M_20# プログラム間共通外部変数
1630             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1631         '
1632         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1633             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1634             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1635             Break
1636         '
1637         EndIf
1638         '
1639         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1640         '
1641     EndIf
1642     '
1643     If M_20# = MContinue% Then *RETRY_PCBCHECK
1644 FEnd
1645 '
1646 '■ScrewTight_S2
1647 ''' <summary>
1648 ''' ねじ締めを行う
1649 ''' </summary>
1650 '''<param name="PScrewPos()">
1651 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1652 '''             PScrewPos(2)    ：ねじ締め回避点
1653 '''             PScrewPos(10)   ：ねじ締め終了高さ
1654 '''</param>
1655 '''<returns>整数
1656 '''         0=異常終了、1=正常終了
1657 '''</returns>
1658 ''' <remarks>
1659 ''' Date   : 2021/07/07 : M.Hayakawa
1660 ''' </remarks>'
1661 Function M% ScrewTight_S2(ByVal PScrewPosition())   'ネジ締め個別設定
1662     ScrewTight_S2 = 0
1663     MOKNGFlg = 0
1664     Ovrd 100
1665     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1666     ' 暫定
1667     Ovrd 5
1668     Mvs PScrewPosition(10),-10    ' パレット上ねじ締めS①の上空へ移動
1669 '    Ovrd MOvrdA
1670     '暫定マスク
1671 '    M_Out(Y62_Driver)=1     ' バンクセッティング　C1
1672 '    Dly 0.1
1673 '    M_Out(Y61_Driver)=1     'ドライバーON　CW
1674 '    'Spd 8.3 '外部ライド100  内部ライド60   'ライド100-40　100%：Spd　15　'ねじ締め速度設定
1675 '    Spd MSpdA               'ネジ締め時Spd個別設定
1676     ' 暫定移動のみ
1677     Mvs PScrewPosition(10)
1678 '    '
1679 '    Dly 0.1
1680 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1681 '    Wait M_In(11584)=1          '完了/エラー検出
1682 '    Dly 0.1
1683 '    Spd M_NSpd
1684 '    '
1685 '    If M_In(X28_Driver)=1 Then  'ねじトータルエラー検出時
1686 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1687 '        Dly 0.1
1688 '        M_Out(Y62_Driver)=0     'バンクセッティング解除　C1
1689 '        Dly 0.1
1690 '        M_Out(Y63_Driver)=0     'バンクセッティング解除　C1
1691 '        Dly 0.1
1692 '        M_Out(Y65_Driver)=0     'プログラム解除　F1
1693 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1694 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
1695 '        MOKNGFlg = -1
1696 '        ScrewTight_S2 = 0
1697 '    Else
1698 '        Wait M_In(X29_Driver)=1 ' 正常完了時
1699 '        Dly 0.1
1700 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1701 '        Dly 0.1
1702 '        M_Out(Y62_Driver)=0     'バンクセッティング解除
1703 '        Dly 0.1
1704 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
1705 '        Dly 0.1
1706 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1707 '        ScrewTight_S2 = 1
1708 '    EndIf
1709 ' 暫定
1710     Ovrd 10
1711     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1712     Ovrd 100
1713 FEnd
1714 '
1715 '■ScrewGet_S3
1716 ''' <summary>
1717 ''' ねじ供給機からねじを得る
1718 ''' </summary>
1719 '''<param name="%"></param>
1720 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1721 '''         PScrewPos(2)    ：ねじ供給器回避点
1722 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1723 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1724 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1725 '''<returns>整数
1726 '''         0=異常終了、1=正常終了、-1=MネジセンサーNG、-2=MネジセンサーON、-3=吸着エラー
1727 '''</returns>
1728 ''' <remarks>
1729 ''' Date   : 2021/07/07 : M.Hayakawa
1730 ''' </remarks>'
1731 Function M% ScrewGet_S3(ByVal PScrewPosition())
1732     ScrewGet_S3 = 0
1733     MMScrewJudge% = 0
1734     'ねじ供給器初期動作エラーチェック
1735 ' ↓暫定削除
1736 '    Wait M_In(X34_ScrewReady1)=1 'ねじ供給器SがReadyになるまで待つ　←　何秒か待ってReadyにならなければ抜けるプログラムが必要？
1737 '    Ovrd 100
1738 '    If M_In(X33_SS2)=0 Then  'Mねじ検出センサがOFF（故障）していた場合
1739 '        Ovrd 30
1740 '        Mvs,-80             'その場所から80mm上空へ移動
1741 '        Mov PInitPos19049   '19049初期位置へ移動
1742 '        M_Out(Y6A_VV1)=0    'ねじ吸着 Off
1743 '        'NGとしてここの関数から抜ける
1744 '        ScrewGet_S3 = -1
1745 '        MMScrewJudge% = 1
1746 '        MCommentD1001 = 61
1747 '    EndIf
1748 '    If ScrewGet_S3 = 0 Then
1749 '        'Sタイト用ねじ供給機にMねじが混入していないか監視
1750 '        MMScrewJudge% = 0 'MMScrewJudgeを初期化する
1751 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
1752 '        If MRtn = 0 Then
1753 '            Ovrd 30
1754 '            Mvs,-80            'その場所から50mm上空へ移動
1755 '            Mov PInitPos19049  '19049初期位置へ移動
1756 '            MMScrewJudge% = 2
1757 '            MRtn = All_CLamp_Release()'全てのクランプ解除へ分岐
1758 '            MCnt% = 2   '2を設定
1759 '            MCommentD1001 = 62
1760 '        EndIf
1761 '        If MMScrewJudge% = 2 Then
1762 '            ScrewGet_S3 = -2
1763 '        EndIf
1764 '    EndIf
1765 '    'Mネジ判定がONの場合 NGとして関数を抜ける
1766 '    If MMScrewJudge% = 2 Then
1767 '        ScrewGet_S3 = -2
1768 '    EndIf
1769     'Sネジ用ねじ太郎のMネジ混入確認用ここまで
1770     Ovrd 100
1771     Spd M_NSpd
1772     If MMScrewJudge% = 0 Then
1773         ScrewGet_S3 = 0
1774         M_Out(Y63_Driver)=1         ' バンクセッティング　C2
1775         MScrewCnt% = 0
1776         MFinCnt% = 2
1777 '        For MCnt% = 0 To MFinCnt%
1778             Mov PScrewPosition(2)        ' ねじ供給機回避点
1779             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1780             Ovrd 80
1781             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1782             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
1783             Mvs PScrewPosition(10), 1.2
1784             M_Out(Y6A_VV1)=1        ' ねじ吸着　ON
1785             'ビット回転
1786             M_Out(Y60_Driver)=1
1787             Dly 0.2
1788             '
1789             Ovrd 100
1790             JOvrd M_NJovrd
1791             Spd M_NSpd
1792             'ネジ吸着確認位置移動
1793             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
1794             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
1795             'ビット回転停止
1796             'M_Out(Y60_Driver)=0
1797             '
1798             '1秒間ネジ吸着確認
1799 ' 以下暫定削除
1800 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
1801 '            'MRtn = 0'強制エラー
1802 '            '吸着エラーの場合
1803 '            'ネジをねじ太郎に戻す
1804 '            If MRtn = 0 Then
1805 '                Ovrd 30
1806 '                'ビット回転停止
1807 '                M_Out(Y60_Driver)=0
1808 '                'ネジ供給機上空
1809 '                Mvs PScrewPos(1)
1810 '                '更に上空
1811 '                Mov PScrewPos(1), -75
1812 '                'ネジ捨て位置
1813 '                Mov PScrewFeedS021
1814 '                '吸着OFF
1815 '                M_Out(Y6A_VV1)=0 'ねじ吸着　OFF
1816 '                Dly 0.2
1817 '                '破壊ON
1818 '                M_Out(Y6B_VB1)=1 '真空破壊ON
1819 '                'ビット回転
1820 '                M_Out(Y61_Driver)=1
1821 '                Dly 0.5
1822 '                '
1823 '                Ovrd 100
1824 '                JOvrd M_NJovrd
1825 '                Spd M_NSpd
1826 '                'ドライバーを上下させねじを振り落とす
1827 '                Mov PScrewFeedS021, 10
1828 '                Mov PScrewFeedS021
1829 '                Dly 0.1
1830 '                Mov PScrewFeedS021, 10
1831 '                Mov PScrewFeedS021
1832 '                '
1833 '                'ネジ落ち待ち
1834 '                'ビット回転停止
1835 '                M_Out(Y61_Driver)=0
1836 '                Dly 0.1
1837 '                '破壊OFF
1838 '                M_Out(Y6B_VB1)=0 '真空破壊OFF
1839 '                '
1840 '                '
1841 '                'ねじ落ちたとして、移動更に上空
1842 '                Mov PScrewPos(1), -75
1843 '                Ovrd 100
1844 '                Spd M_NSpd
1845 '                'ネジ供給機上空
1846 '                Mvs PScrewPos(1)
1847 '                '
1848 '                ScrewGet_S3 = -3
1849 '                Break
1850 '                '
1851 '            Else
1852 '                MCnt% = MFinCnt%
1853 '                ScrewGet_S3 = 0
1854 '            EndIf
1855 '        Next  MCnt%
1856         '
1857         Ovrd 100
1858         Spd M_NSpd
1859         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
1860         M_Out(Y60_Driver)=0     ' ビット回転停止
1861         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
1862         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
1863         'もう一度吸着確認
1864 ' 以下暫定削除
1865 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
1866 '        If MRtn = 0 Then      '吸着エラーの場合
1867 '            MCommentD1001 = 94
1868 '            MCommentD1002 = 95
1869 '            ScrewGet_S3 = -3
1870 '        EndIf
1871 '        If MRtn = 1 Then      '吸着OKの場合
1872 '            ScrewGet_S3 = 1
1873 '        EndIf
1874 '        Break
1875     Else
1876         'Mネジ
1877         If MMScrewJudge% = 2 Then
1878             ScrewGet_S3 = -2
1879         EndIf
1880     EndIf
1881 FEnd
1882 '
1883 '■fnKEY_WAIT()
1884 ''' <summary>
1885 ''' GOTからのキー入力待ち
1886 ''' </summary>
1887 '''<returns>1：停止    2：次へ
1888 '''         3：継続    4：トルクチェック開始
1889 '''         5：NG
1890 '''         11：ロボット初期位置1    12：ロボット初期位置2
1891 '''         13：ロボット初期位置3    14：ロボット初期位置4
1892 '''</returns>
1893 ''' <remarks>
1894 ''' Date   : 2021/07/07 : M.Hayakawa
1895 ''' </remarks>'
1896 Function M% fnKEY_WAIT()
1897     fnKEY_WAIT = 0
1898     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
1899     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
1900     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
1901     '下記キー待ちの継続に反応させないため
1902     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
1903     Dly 0.2
1904     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
1905     MLocalLoopFlg=1
1906     While MLocalLoopFlg=1
1907         If M_In(11345) = 1 Then         '停止   M5345
1908             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
1909             fnKEY_WAIT = 1
1910             MLocalLoopFlg=-1
1911             Break
1912         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
1913             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
1914             fnKEY_WAIT = 2
1915             MLocalLoopFlg=-1
1916             Break
1917         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
1918             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
1919             fnKEY_WAIT = 3
1920             MLocalLoopFlg=-1
1921             Break
1922         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
1923             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
1924             fnKEY_WAIT = 4
1925             MLocalLoopFlg=-1
1926             Break
1927         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
1928             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
1929             fnKEY_WAIT = 5
1930             MLocalLoopFlg=-1
1931             Break
1932             '
1933         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
1934             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
1935             fnKEY_WAIT = MRobotInit1%
1936             MLocalLoopFlg=-1
1937             Break
1938             '
1939         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
1940             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
1941             fnKEY_WAIT = MRobotInit2%
1942             MLocalLoopFlg=-1
1943             Break
1944             '
1945         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
1946             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
1947             fnKEY_WAIT = MRobotInit3%
1948             MLocalLoopFlg=-1
1949             Break
1950             '
1951         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
1952             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
1953             fnKEY_WAIT = MRobotInit4%
1954             MLocalLoopFlg=-1
1955             Break
1956             '
1957         Else
1958         EndIf
1959     WEnd
1960     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
1961     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
1962 FEnd
1963 '
1964 '■ fnAUTO_CTL
1965 ''' <summary>
1966 ''' AUTOモードOFF、PLCからの開始待ち
1967 ''' </summary>
1968 ''' <remarks>
1969 ''' Date   : 2021/07/07 : M.Hayakawa
1970 ''' </remarks>
1971 Function M% fnAUTO_CTL
1972     fnAUTO_CTL = 0
1973     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1974     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
1975     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1976     '
1977     If M_Svo=0 Then             'サーボON確認
1978         Servo On
1979     EndIf
1980     Wait M_Svo=1
1981 FEnd
1982 '
1983 '■ fnWindScreenOpen
1984 ''' <summary>
1985 ''' ウィンド画面の表示、非表示設定
1986 ''' </summary>
1987 '''<param name="%"></param>
1988 '''<param name="%"></param>
1989 '''<param name="%"></param>
1990 '''<param name="%"></param>
1991 ''' <remarks>
1992 ''' コメントD1001, D1002, D1003の設定
1993 ''' MWindReSet = 0     画面非表示
1994 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
1995 ''' MWindErrScr = 10    エラー画面 D1001, D1002
1996 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
1997 ''' Date   : 2021/07/07 : M.Hayakawa
1998 ''' </remarks>
1999 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2000     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2001         M_Out16(12480) = MCommentD1001            'D1001 コメント
2002     EndIf
2003     '
2004     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2005         M_Out16(12496) = MCommentD1002            'D1002 コメント
2006     EndIf
2007     '
2008     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2009        M_Out16(12512) = MCommentD1003            'D1003 コメント
2010     EndIf
2011     '
2012     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2013     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2014     Dly 0.5
2015     M_Out(12363) = 0                         'ウィンド画面設定
2016 FEnd
2017 '
2018 '■FnCtlValue2
2019 ''' <summary>
2020 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2021 ''' </summary>
2022 ''' <param name="MCtlNo%"></param>
2023 ''' <remarks>
2024 ''' Date : 2022/04/28 渡辺
2025 ''' </remarks>
2026 '''
2027 '''  1：投入数       ＋１
2028 '''  2：組立ＯＫ数   ＋１
2029 '''  3：組立ＮＧ数   ＋１ (未使用)
2030 '''  4：吸着エラー数 ＋１
2031 ''' 99：読書開始信号 OFF
2032 '''
2033 Function M% FnCtlValue2(ByVal MCtlNo%)
2034     FnCtlValue2 = 1
2035     Select MCtlNo%
2036         Case 1        '投入数＋１
2037             M_Out(12569) = 0             '書込み開始信号OFF
2038             M_Out(12568) = 1             '読込み開始信号ON
2039             MInputQty = M_In16(11600)    '投入数受信
2040             MInputQty = MInputQty + 1    '投入数＋１
2041             M_Out16(12592) = MInputQty   '投入数送信
2042             M_Out(12569) = 1             '書込み開始信号ON
2043             Break
2044             '
2045         Case 2        '組立ＯＫ数＋１
2046             M_Out(12569) = 0             '書込み開始信号OFF
2047             M_Out(12568) = 1             '読込み開始信号ON
2048             MAssyOkQty = M_In16(11616)   '組立OK数受信
2049             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2050             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2051             M_Out(12569) = 1             '書込み開始信号ON
2052             Break
2053             '
2054         Case 4        '吸着エラー数＋１
2055             M_Out(12569) = 0                       '書込み開始信号OFF
2056             M_Out(12568) = 1                       '読込み開始信号ON
2057             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2058             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2059             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2060             M_Out(12569) = 1                       '書込み開始信号ON
2061             Break
2062             '
2063         Case 99        '読書開始信号OFF
2064             M_Out(12568) = 0        '読込み開始信号OFF
2065             M_Out(12569) = 0        '書込み開始信号OFF
2066             Break
2067             '
2068     End Select
2069     Exit Function
2070 FEnd
2071 'Insightによる画像処理検査実行（並列処理なし）
2072 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2073 '-------------------------------------------------------------------------------
2074 'Insightによる画像処理検査実行（並列処理なし）
2075 '   引数
2076 '       PInspPos()      ：検査位置
2077 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2078 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2079 '       MInspCnt%       ：検査位置数
2080 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2081 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2082 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2083 '   戻り値：整数
2084 '       0=異常終了、1=正常終了
2085 '
2086 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2087 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2088 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2089 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2090 '   20200410    :   検査グループ設定Retry追加
2091 '-------------------------------------------------------------------------------
2092     '----- 初期設定 -----
2093     Cnt 0                                                           '移動効率化解除(初期値=0)
2094     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2095 '    Cnt 1,0.1,0.1
2096     '変数宣言・初期化
2097     Def Inte MNum                                                   '検査番号(検査順1～)
2098     MNum% = 1                                                       '検査番号初期値設定
2099     Def Inte MEndFlg                                                '検査終了フラグ
2100     MEndFlg% = 0
2101     '
2102     '検査G番号設定要求・検査実行要求off
2103     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2104     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2105     'エラー番号クリア
2106     MInspErrNum = 0                                                 '検査実行エラー番号
2107     M_Out16(MOUT_InspErrNum) = MInspErrNum
2108     MInspNGStepNum = 0                                              '検査実行NGStep番号
2109     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2110     '
2111     'Insight Ready check?
2112     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2113         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2114         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2115         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2116         ISInspectionSingle = 0                                      '異常終了戻り値設定
2117         Exit Function
2118     EndIf
2119     '
2120     '検査位置数確認
2121     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2122         MInspErrNum = 21                                            '検査データなし 21　引数<1
2123         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2124         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2125         ISInspectionSingle = 0                                      '異常終了戻り値設定
2126         Exit Function
2127     EndIf
2128     '
2129     '
2130     '
2131     '----- メイン処理 -----
2132     '設定された検査位置数分の検査実行
2133     While( MEndFlg% = 0 )
2134         '----- 検査グループ番号設定Retry追加 20200410
2135         MSetGrNumRetryExitFlg = 0
2136         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2137         While( MSetGrNumRetryExitFlg = 0 )
2138         '----- 検査グループ番号設定Retry追加ここまで 20200410
2139             '
2140             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2141             '
2142             '----- 検査グループ番号設定 -----
2143             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2144             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2145             '
2146             '検査位置へ移動・移動完了待ち
2147             Mvs PInspPos( MNum% )                                       '移動
2148             Dly 0.05                                                    '移動完了後Delay
2149             '
2150             '検査グループ番号設定終了確認
2151             M_Timer(1) = 0
2152             MExitFlg = 0
2153             While( MExitFlg = 0 )
2154                 '検査G設定正常終了?
2155                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2156                     MExitFlg = 1
2157                 '
2158                 '検査G設定異常終了?
2159                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2160                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2161                     If MInspErrNum = 0 Then                             '1回目のエラー?
2162                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2163                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2164                     EndIf
2165                     MExitFlg = 1
2166                 '
2167                 'timeoutチェック
2168                 ElseIf 1000 < M_Timer(1) Then
2169                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2170                     If MInspErrNum = 0 Then                             '1回目のエラー?
2171                         MInspErrNum = 12                                'timeout エラー番号=12
2172                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2173                     EndIf
2174                     MExitFlg = 1
2175                 EndIf
2176             WEnd
2177             '
2178             '検査G番号設定要求off
2179             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2180             '
2181             '----- 検査グループ設定Retry追加 20200410
2182             'NGなければ抜ける
2183             If MCurrentStepErr = 0 Then
2184                 MSetGrNumRetryExitFlg = 1
2185             Else
2186                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2187                 If MSetGrNumRetryCnt = 0 Then
2188                     MSetGrNumRetryExitFlg = 1
2189                 Else
2190                     'Retryへ　その前にDelay
2191                     Dly 0.5
2192                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2193                 EndIf
2194             EndIf
2195             '----- 検査グループ設定Retry追加ここまで 20200410
2196             '
2197         WEnd
2198         '
2199         '
2200         '
2201         '----- 検査実行 -----
2202         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2203             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2204                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2205                 MInspRetryExitFlg = 0
2206                 MRetryCnt = 2                                        'Retry回数設定
2207                 While( MInspRetryExitFlg = 0 )
2208                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2209                     '
2210                     '検査完了確認
2211                     MRetryCnt = MRetryCnt - 1
2212                     M_Timer(1) = 0
2213                     MExitFlg = 0
2214                     While( MExitFlg = 0 )
2215                     '検査完了待ち
2216                         '検査OK終了?
2217                         If M_In( MIN_IS_InspOK% ) = 1  Then
2218                             MJudgeOKFlg = 1                         '検査OKフラグON
2219                             MExitFlg = 1
2220                         '
2221                         '検査NG終了?
2222                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2223                             If MInspErrNum = 0 Then                 '1回目のエラー?
2224                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2225                                     MInspErrNum = 32                    '検査NG エラー番号=32
2226                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2227                                 EndIf
2228                             EndIf
2229                             MExitFlg = 1
2230                         '
2231                         '検査異常終了(IS timeout)?
2232                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2233                             If MInspErrNum = 0 Then                 '1回目のエラー?
2234                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2235                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2236                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2237                                 EndIf
2238                             EndIf
2239                             MExitFlg = 1
2240                         '
2241                         'timeoutチェック
2242                         ElseIf 3000 < M_Timer(1) Then
2243                             If MInspErrNum = 0 Then                 '1回目のエラー?
2244                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2245                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2246                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2247                                 EndIf
2248                             EndIf
2249                             MExitFlg = 1
2250                         EndIf
2251                     WEnd
2252                     '
2253                     '検査開始要求off
2254                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2255                     '
2256                     'OKなら抜ける
2257                     If MJudgeOKFlg = 1 Then
2258                         MInspRetryExitFlg = 1
2259                     Else
2260                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2261                         If MRetryCnt = 0 Then
2262                             MInspRetryExitFlg = 1
2263                         Else
2264                             'Retryへ　その前にDelay
2265                             Dly 0.3
2266                         EndIf
2267                     EndIf
2268                     '
2269                 WEnd
2270             EndIf
2271         EndIf
2272         '
2273         '
2274         '
2275         MNum% = MNum% + 1                                           '検査Step+1
2276         '検査終了確認　検査終了フラグセット
2277         If (MInspCnt% < MNum% ) Then
2278             MEndFlg% = 1                                            '検査終了フラグセット
2279         EndIf
2280         'NG発生時続行時処理
2281         If MInspErrNum <> 0 Then                                    'NGあり?
2282             If MNgContinue% <> 1 Then                               'NG続行?
2283                 MEndFlg% = 1                                        '検査終了フラグセット
2284             EndIf
2285         EndIf
2286     WEnd
2287     '
2288     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2289     If 0 < MZAxis% Then
2290         PCurrentPos = P_Curr                                        '現在位置取得
2291         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2292         Mvs PCurrentPos                                             '現在位置上空へ移動
2293     EndIf
2294     Fine 0 , P
2295     '
2296     '戻り値設定
2297     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
2298         ISInspectionSingle = 1                                      '正常終了戻り値設定
2299     Else
2300         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2301         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2302         ISInspectionSingle = 0                                      '異常終了戻り値設定
2303     EndIf
2304     '
2305 FEnd
2306 '
2307 ' ■ISInspection
2308 ''' <summary>
2309 ''' Insightによる画像処理検査実行
2310 ''' </summary>
2311 '''<param name="PInspPos()">検査位置</param>
2312 '''<param name="MInspGrNum%()">検査位置での検査グループ番号（=0：画像検査未実施）</param>
2313 '''             PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2314 '''<param name="MInspCnt%">検査位置数</param>
2315 '''<param name="MZAxis%">終了時のZ軸退避座標（-1:無効）</param>
2316 '''             終了時にZ軸をMZAxisで設定された位置まで上昇させる
2317 '''<param name="MNgContinue%">=1で検査エラー・NG発生時に全Stepの検査を行う</param>
2318 '''<returns>    整数 0=異常終了、1=正常終了</returns>
2319 '''         MInspErrNum     ：異常終了時にエラー番号が設定される
2320 '''         MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される"
2321 ''' <remarks>
2322 ''' Date   : 2021/07/07 : M.Hayakawa
2323 ''' </remarks>
2324 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2325 '    '画像使用確認 0<- 画像確認無しの場合
2326 '    If M_In(11369) = 0 Then            'toRBT_使用確認
2327 '        ISInspection = 1                                        '正常終了戻り値設定
2328 '    EndIf
2329 ''
2330 '    Cnt 0                                                       '移動効率化解除(初期値=0)
2331 '    Fine 0.05,P                                                 '位置決め完了条件設置　0.05mm
2332 '    MNum% = 1                                                   '検査番号初期値設定
2333 '    Def Inte MEndFlg                                            '検査終了フラグ
2334 '    MEndFlg% = 0
2335 '    '
2336 '    'エラー番号クリア
2337 '    MInspErrNumSub = 0                                          '検査実行エラー番号sub
2338 '    MInspErrNum = 0                                             '検査実行エラー番号
2339 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2340 '    MInspNGStepNum = 0                                          '検査実行NGStep番号
2341 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2342 '    '
2343 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready offなら終了
2344 '        MInspErrNum = 20                                        '検査実行エラー番号 20 Insight offline
2345 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2346 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2347 '        ISInspection = 0                                        '異常終了戻り値設定
2348 ''
2349 '    EndIf
2350 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2351 '    '
2352 '    '検査位置数確認
2353 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2354 '        MInspErrNum = 21                                        '検査データなし 21　引数<1
2355 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2356 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2357 '        ISInspection = 0                                        '異常終了戻り値設定
2358 ''
2359 '    EndIf
2360 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2361 '    '
2362 '    '設定された検査位置数分の検査実行
2363 '    While( MEndFlg% = 0 )
2364 '        '検査終了確認　検査終了フラグセット
2365 '        If (MInspCnt% < MNum% ) Then
2366 '            MEndFlg% = 1                                        '検査終了フラグセット
2367 '        EndIf
2368 '        '
2369 '        'タスク　検査G番号設定・検査完了確認処理開始　INSPTAST1
2370 '        If MEndFlg% = 0 Then
2371 '            M_01# = MInspGrNum%(MNum%)                          '検査G番号引渡し
2372 '        EndIf
2373 '        M_02# = MEndFlg%                                        '検査終了フラグ引渡し
2374 '        M_05# = MNum%                                           '検査番号(検査順1～)
2375 '        'タスク　検査G設定フラグ引渡し
2376 '        If MEndFlg% = 0 Then
2377 '            If 0 < MInspGrNum%(MNum%) Then
2378 '                M_03# = 1
2379 '            Else
2380 '                M_03# = 0
2381 '            EndIf
2382 '        Else
2383 '            M_03# = 0
2384 '        EndIf
2385 '        'タスク　検査結果確認フラグ引渡し
2386 '        If 1 < MNum% Then
2387 '            If 0 < MInspGrNum%(MNum%-1) Then
2388 '                M_04# = 1
2389 '            Else
2390 '                M_04# = 0
2391 '            EndIf
2392 '        Else
2393 '            M_04# = 0
2394 '        EndIf
2395 '        '
2396 '        'タスク処理開始
2397 '        M_00# = 1                                               'TASK処理開始
2398 '        'タスク処理開始確認
2399 '        M_Timer(1) = 0
2400 '        MExitFlg = 0
2401 '        While( MExitFlg = 0 )
2402 '            '処理開始完了確認
2403 '            If M_00# = 0 And M_10# = 8 Then
2404 '                MExitFlg = 1
2405 '            EndIf
2406 '            'timeoutチェック
2407 '            If 2000 < M_Timer(1) Then
2408 '                If MNgContinue% = 1 Then                        'NG続行?
2409 '                    MInspErrNumSub = 36                         'エラー番号設定36
2410 '                Else
2411 '                    MInspErrNum = 36                            'エラー番号設定36
2412 '                EndIf
2413 '                MExitFlg = 1
2414 '            EndIf
2415 '        WEnd
2416 '        '
2417 '        '検査位置へ移動・移動完了待ち
2418 '        If 0 = MInspErrNum Then
2419 '            If MEndFlg% = 0 Then
2420 '                Mvs PInspPos( MNum% )                           '移動
2421 '            EndIf
2422 '        EndIf
2423 '        '
2424 '        'タスク　検査G番号設定・検査完了確認処理終了待ち　INSPTAST1
2425 '        If 0 = MInspErrNum Then
2426 '            M_Timer(1) = 0
2427 '            MExitFlg = 0
2428 '            While( MExitFlg = 0 )
2429 '                '処理完了待ち（正常終了）
2430 '                If M_10# = 1 Then
2431 '                    MExitFlg = 1
2432 '                EndIf
2433 '                '処理完了待ち（異常終了）
2434 '                If M_10# = 0 Then
2435 '                    If MNgContinue% = 1 Then                    'NG続行?
2436 '                        MInspErrNumSub = M_12#                  'エラー番号設定　M12
2437 '                    Else
2438 '                        MInspErrNum = M_12#                     'エラー番号設定　M12
2439 '                    EndIf
2440 '                    MExitFlg = 1
2441 '                EndIf
2442 '                'timeoutチェック
2443 '                If 5000 < M_Timer(1) Then
2444 '                    If MNgContinue% = 1 Then                    'NG続行?
2445 '                        MInspErrNumSub = 31                     'エラー番号設定31
2446 '                    Else
2447 '                        MInspErrNum = 31                        'エラー番号設定31
2448 '                    EndIf
2449 '                    MExitFlg = 1
2450 '                EndIf
2451 '            WEnd
2452 '        EndIf
2453 '        '
2454 '        '検査結果確認
2455 '        If 0 = MInspErrNum Then
2456 '            If 1 < MNum% Then
2457 '                If 0 < MInspGrNum%(MNum%-1) Then                '検査あり?
2458 '                    If M_11# = 2 Then                           '検査NG?
2459 '                        If MNgContinue% = 1 Then                'NG続行?
2460 '                            If MInspNGStepNum = 0 Then          'NG未発生?
2461 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2462 '                            EndIf
2463 '                            MInspErrNumSub = 32                 'エラー番号設定 32:検査NG
2464 '                        Else
2465 ''                            MInspNGStepNum = MNum% - 1          '検査実行NGStep番号設定
2466 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2467 '                            MInspErrNum = 32                    'エラー番号設定 32:検査NG
2468 '                        EndIf
2469 '                   EndIf
2470 '                EndIf
2471 '            EndIf
2472 '        EndIf
2473 '        '
2474 '        'エラーなら検査中断終了するのでLoopから抜けるため終了フラグセット
2475 '        If 0 <> MInspErrNum Then
2476 '            MEndFlg% = 1
2477 '        EndIf
2478 '        '
2479 '        '検査実行、取込完了待ち
2480 '        If 0 = MInspErrNum Then
2481 '            If MEndFlg% = 0 Then
2482 '                If 0 < MInspGrNum%(MNum%) Then                  '検査あり?
2483 '                    M_Out(MOUT_IS_Insp%) = 1                    '検査実行要求on
2484 '                    '取込完了確認
2485 '                    M_Timer(1) = 0
2486 '                    MExitFlg = 0
2487 '                    While( MExitFlg = 0 )
2488 '                        '処理完了待ち
2489 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2490 '                            MExitFlg = 1
2491 '                        EndIf
2492 '                        'timeoutチェック
2493 '                        If 2000 < M_Timer(1) Then
2494 '                            If MNgContinue% = 1 Then            'NG続行?
2495 '                                MInspErrNumSub = 33             'エラー番号設定33
2496 '                            Else
2497 '                                MInspErrNum = 33                'エラー番号設定33
2498 '                            EndIf
2499 '                            MExitFlg = 1
2500 '                        EndIf
2501 '                    WEnd
2502 '                EndIf
2503 '                '
2504 '            EndIf
2505 '        EndIf
2506 '        MNum% = MNum% + 1
2507 '    WEnd
2508 '    '
2509 '    '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2510 '    If 0 < MZAxis% Then
2511 '        PCurrentPos = P_Curr                                    '現在位置取得
2512 '        PCurrentPos.Z = MZAxis%                                 'Z軸を設定
2513 '        Mvs PCurrentPos                                         '現在位置上空へ移動
2514 '    EndIf
2515 '    '
2516 '    'NG続行時処理
2517 '    If MNgContinue% = 1 Then                                    'NG続行?
2518 '        MInspErrNum = MInspErrNumSub                            'エラー番号設定
2519 '    EndIf
2520 '    '
2521 '    '戻り値設定
2522 '    If MInspErrNum = 0 Then
2523 '        ISInspection = 1                                        '正常終了戻り値設定
2524 '    Else
2525 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2526 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2527 '        ISInspection = 0                                        '異常終了戻り値設定
2528 '    EndIf
2529 '    '
2530 '*ISInspection_End
2531 'FEnd
2532 '
2533 '■InitialZoneB
2534 ''' <summary>
2535 ''' 非常停止後の復帰動作
2536 ''' 1)上空退避　Z方向上に移動
2537 ''' 2)J1軸以外を退避ポジションへ移動
2538 ''' 3)J1軸のみを退避ポジションへ移動
2539 ''' 4)イニシャルポジションへ移動
2540 ''' </summary>
2541 ''' <remarks>
2542 ''' Date : 2022/04/08 : N.Watanabe
2543 ''' </remarks>
2544 Function V fnInitialZoneB()
2545     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/04/26 渡辺
2546 '
2547 'パラメータ
2548     Ovrd 5
2549 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2550 '    Cmp Pos, &B100011
2551 '
2552 '復帰動作開始
2553 '
2554 '置き台と両掴みの場所は、チャックを解放する
2555 *RecoveryChuckOpen
2556     PActive = P_Curr          '現在位置を取得
2557     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
2558 'PProductOnRoboSet(ねじロボ製品置き位置)は、チャック解放
2559     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2560         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2561             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2562                 MRecoveryChuckOpen = 1
2563             EndIf
2564         EndIf
2565     EndIf
2566 'PProductOnRoboGet(ねじロボ製品取り位置)は、チャック解放
2567     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2568         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2569             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2570                 MRecoveryChuckOpen = 1
2571             EndIf
2572         EndIf
2573     EndIf
2574 '
2575     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2576     M_Out(12256) = 0                           '本体チャック閉OFF
2577     M_Out(12257) = 1                           '本体チャック開ON
2578 '
2579     M_20# = 0                                  'KEY入力初期化
2580     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
2581     If MRtn = 1 Then M_Out(12257) = 0          '本体チャック開OFF
2582     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2583 '
2584     fErrorProcess(11,244,284,0)
2585     If M_20# = MNext% Then M_20# = MClear%
2586     If M_20# = MAbout% Then GoTo *RecoveryEnd
2587     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2588     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2589 '
2590     *RecoveryChuckOpenEnd
2591 '
2592 '背面板回避
2593 'PPlateBackSet～PPlateBackSet_6のエリアにいるときは、本体チャック開く
2594 '・PPlateBackSet_6         '経路6
2595 '・PPlateBackSet_5         '経路7
2596 '・PPlateBackSet_4         '経路8
2597 '・PPlateBackSet_3         '経路9
2598 '・PPlateBackSet_2         '経路10
2599 '・PPlateBackSet_1         '経路11
2600 '・PPlateBackSet           '背面板置き位置
2601 '上記７点のＸ座標・Ｙ座標・Ｚ座標とJ6軸が下記If文の範囲に入っている事を確認する事
2602     PActive = P_Curr                    '現在位置を取得
2603     JActive = J_Curr                    '現在位置を取得
2604     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2605     If (PActive.X >= -35) And (PActive.X <= -5) Then
2606         If (PActive.Y >= 350) And (PActive.Y <= 515) Then
2607             If (PActive.Z >= 480) And (PActive.Z <= 560) Then
2608                 If (MJ6 >= 160) And (MJ6 <= 200) Then
2609                     M_Out(12256) = 0            '本体チャック閉OFF
2610                     M_Out(12257) = 1            '本体チャック開ON
2611                 Dly 1.0
2612                 EndIf
2613             EndIf
2614         EndIf
2615     EndIf
2616 '
2617 '
2618 '特殊回避　直接、上空退避が出来ない所の対処
2619 '
2620     Ovrd 1
2621 'PProductOnRoboSet(Get)～PProductOnRoboSet(Get)_2のエリアにいるときは、PProductOnRoboSet_2へ
2622 '・PProductOnRoboSet
2623 '・PProductOnRoboSet_1
2624 '・PProductOnRoboSet_2
2625 '・PProductOnRoboGet
2626 '・PProductOnRoboGet_1
2627 '・PProductOnRoboGet_2
2628 '上記６点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
2629     PActive = P_Curr                    '現在位置を取得
2630     JActive = J_Curr                    '現在位置を取得
2631     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2632     If (PActive.X >= -30) And (PActive.X <= 0) Then
2633         If (PActive.Y >= 380) And (PActive.Y <= 420) Then
2634             If (PActive.Z >= 300) And (PActive.Z <= 450) Then
2635                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2636                     Mvs PProductOnRoboSet_1
2637                     Dly 1.0
2638                     Mvs PProductOnRoboSet_2
2639                     Dly 1.0
2640                     Mov PProductOnRoboSet_3
2641                     Dly 1.0
2642                 EndIf
2643             EndIf
2644         EndIf
2645     EndIf
2646 '
2647 'PProductOnRoboSet(Get)_2～PProductOnRoboSet(Get)_3のエリアにいるときは、PProductOnRoboSet_3へ
2648 '・PProductOnRoboSet_2
2649 '・PProductOnRoboSet_3
2650 '・PProductOnRoboGet_2
2651 '・PProductOnRoboGet_3
2652 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
2653     PActive = P_Curr                    '現在位置を取得
2654     JActive = J_Curr                    '現在位置を取得
2655     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2656     If (PActive.X >= -35) And (PActive.X <= 0) Then
2657         If (PActive.Y >= 280) And (PActive.Y <= 400) Then
2658             If (PActive.Z >= 410) And (PActive.Z <= 530) Then
2659                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2660                     Mvs PProductOnRoboSet_3
2661                     Dly 1.0
2662                 EndIf
2663             EndIf
2664         EndIf
2665     EndIf
2666 '
2667     Ovrd 5
2668 '
2669 '上空退避
2670     PActive = P_Curr
2671     Pmove = PActive
2672     Pmove.Z = 640           '上空退避する一律の高さ
2673     If PActive.X > 550 Then
2674         Pmove.Z =550        'パレット上に腕を伸ばしているときは640まで上げられない為、例外処置
2675     EndIf
2676     If PActive.Z < Pmove.Z Then
2677         Mvs Pmove
2678     EndIf
2679     Dly 1.0
2680 'J1軸以外を退避ポジションへ移動
2681     JActive = J_Curr
2682     Jmove = JTaihi
2683     Jmove.J1 = JActive.J1        'J1軸は現在値を使用し、JTaihiのポーズを取る
2684     Jmove.J6 = JActive.J6        'J6軸は現在値を使用し、JTaihiのポーズを取る
2685     Mov Jmove
2686     Dly 1.0
2687 'J1軸のみを退避ポジションへ移動
2688     Mov JTaihi
2689     Dly 1.0
2690 'イニシャルポジションへ移動
2691     Mov PInitialPosition
2692     Cmp Off
2693     Ovrd 100
2694 ' ねじロボを初期位置に戻すために強制的に自動運転開始
2695     If M_In(11856) = 0 Then                 ' 停止中のみ
2696         fnAutoScreenComment(501)            ' 状態表示[ネジ締め機自動運転開始中] 2022/04/25 渡辺
2697         M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
2698         MRet = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
2699         If MRet = 0 Then
2700         Else
2701             M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
2702         EndIf
2703     EndIf
2704     M_Out(12262) = 0            '位置決め出OFF
2705     M_Out(12263) = 1            '位置決め戻ON
2706     fErrorProcess(11,253,281,0)
2707 *RecoveryEnd
2708     Exit Function
2709 FEnd
2710 '
2711 '
2712 '■fnAutoScreenComment
2713 ''' <summary>
2714 ''' メイン画面の動作状況表示
2715 ''' コメントD1005の設定
2716 ''' </summary>
2717 '''<param name="McommentD1005%">コメントID</param>
2718 ''' <remarks>
2719 ''' Date   : 2021/07/07 : M.Hayakawa
2720 ''' </remarks>
2721 Function fnAutoScreenComment(ByVal McommentD1005%)
2722     M_Out16(12576) = McommentD1005%
2723 FEnd
2724 '
2725 '■fnRoboPosChk
2726 ''' <summary>
2727 ''' 最後に終了したロボットポジションの確認
2728 ''' </summary>
2729 '''<param name="MINNumber%">入力番号</param>
2730 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2731 '''<param name="MTimeCnt&">タイムアウト時間</param>
2732 ''' PLCに保続した番号を読込み、確認
2733 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2734 '''<returns>整数 0:タイムアウト 1:OK</returns>
2735 ''' <remarks>
2736 ''' Date   : 2021/07/07 : M.Hayakawa
2737 ''' </remarks>
2738 Function M% fnRoboPosChk
2739     fnRoboPosChk = 0
2740     MRet = fnStepRead()
2741     '初期位置でないと判断した場合
2742     'ウィンド画面切換え
2743     If MRBTOpeGroupNo > 5 Then
2744         '下記キー待ちの継続に反応させないため
2745         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2746         Dly 0.2
2747         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2748         Dly 1.5
2749         '
2750         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2751         '
2752         MLoopFlg% = 1
2753         While MLoopFlg% = 1
2754             '
2755             '
2756             MKeyNumber% = fnKEY_WAIT()
2757             Select MKeyNumber%
2758                 Case Is = MAbout%       '停止
2759                     M_20# = MAbout%
2760                     MLoopFlg% = -1
2761                     Break
2762                 Case Is = MNext%        '次へ
2763                     'MLoopFlg% = -1
2764                     Break
2765                 Case Is = MContinue%    '継続
2766                     M_20# = MContinue%
2767                     MLoopFlg% = -1
2768                     Break
2769                 Default
2770                     Break
2771             End Select
2772         WEnd
2773     EndIf
2774     '
2775     If M_20# = MContinue% Then                              '継続ボタンが押された場合
2776         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
2777         Ovrd 5                                   '低速オーバーライド値設定
2778         Select MRBTOpeGroupNo
2779             Case Is = 5                          '何もしない
2780                 Break
2781             Case Is = 10                         '初期位置へ戻す
2782                 'Mov PTEST001
2783                 Break
2784             Case Is = 15                         '初期位置へ戻す
2785                 'Mov PTEST002
2786                 Dly 0.5
2787                 'Mov PTEST001
2788                 Dly 0.5
2789                 Break
2790             Default
2791                 Break
2792         End Select
2793         '
2794         Ovrd M_NOvrd                            'システムの初期値を設定
2795         M_Out(12364) = 1                        'toPLC_データ保存ON
2796         MRBTOpeGroupNo = 5
2797         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
2798         Dly 1.0
2799         M_Out(12364) = 0                        'toPLC_データ保存OFF
2800         fnRoboPosChk = 1                        '初期位置動作実行
2801         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
2802     EndIf
2803     Exit Function
2804 FEnd
2805 '
2806 '■frInCheck
2807 ''' <summary>
2808 ''' センサーINチェック
2809 ''' </summary>
2810 '''<param name="MINNumber%">入力番号</param>
2811 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2812 '''<param name="MTimeCnt&">タイムアウト時間</param>
2813 '''<returns>整数 0:タイムアウト 1:OK</returns>
2814 ''' <remarks>
2815 ''' Date   : 2021/07/07 : M.Hayakawa
2816 ''' </remarks>
2817 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2818     M_Timer(4) = 0
2819     MloopFlg = 0
2820     While MloopFlg = 0
2821         MCrtTime& = M_Timer(4)
2822         If M_In(MINNumber%) = MCMPFLG% Then
2823             MloopFlg = 1
2824             frInCheck = 1
2825         ElseIf MCrtTime& > MTimeCnt& Then
2826             MloopFlg = 1
2827             frInCheck = 0
2828         EndIf
2829     WEnd
2830 FEnd
2831 '-----------------------------------------------
2832 '
2833 'ねじ締め機通信確認
2834 '
2835 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2836 'fScrewTcomChk = 0　：正常終了
2837 '          　 　 -1 ：異常終了
2838 '-----------------------------------------------
2839 Function M% fScrewTcomChk
2840 *ReCheckScewTcomChk
2841     fScrewTcomChk = 0
2842     '通信確認送信
2843     M_Out(MOUT_ScwT_ComChk%) = MOn%
2844     '通信確認受信待機
2845 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
2846     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
2847     '通信確認送信終了
2848     M_Out(MOUT_ScwT_ComChk%) = MOff%
2849     If MRtn = 0 Then
2850         fScrewTcomChk = -1
2851     EndIf
2852     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
2853  '
2854 FEnd
2855 '
2856 '
2857 '-----------------------------------------------
2858 '
2859 'ねじ締め開始送信
2860 '
2861 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2862 'fScrewTStart = 0　：正常終了
2863 '           　　-1 ：異常終了
2864 '-----------------------------------------------
2865 Function M% fScrewTStart
2866     fScrewTStart = 0
2867     nRet% = 0
2868     'ねじ締め開始待機を受信
2869 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
2870     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
2871     If MRtn = 0 Then nRet% = -1
2872     If MRtn = 0 Then GoTo *ScrewStartERROR      '開始できなかった場合ジャンプ
2873     Dly 0.1
2874     'ねじ締め開始受信を送信
2875     M_Out(MOUT_ScwT_ST%) = MOn%
2876     Dly 0.5
2877     'Wait M_In(MTEST_KEY%) = MOn%
2878     'ねじ締め開始送信終了
2879     M_Out(MOUT_ScwT_ST%) = MOff%
2880     '
2881 *ScrewStartERROR
2882     fScrewTStart = nRet%
2883 FEnd
2884 '
2885 '
2886 '
2887 '-----------------------------------------------
2888 '
2889 'ねじ締め完了受信
2890 '
2891 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2892 'fScewTFinish = 0　：正常終了
2893 '          　 　-1 ：異常終了
2894 '-----------------------------------------------
2895 Function M% fScewTFinish
2896 *ReCheckScewTFinish
2897     fScewTFinish = 0
2898     'ねじ締め完了待機を受信
2899 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
2900     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
2901     If MRtn = 0 Then
2902         fScewTFinish = -1
2903     EndIf
2904     If MRtn = 2 Then GoTo *ReCheckScewTFinish
2905     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
2906     Dly 0.1
2907     'ねじ締め完了受信を送信
2908     M_Out(MOUT_ScwT_FinOK%) = MOn%
2909     Dly 0.5                          'とりあえず保持時間0.5msec
2910     'ねじ締め開始送信終了
2911     M_Out(MOUT_ScwT_FinOK%) = MOff%
2912     'Wait M_In(MTEST_KEY%) = MOn%
2913     '
2914 *ScewTFinish_ErrEnd
2915 FEnd
2916 '
2917 '
2918 '-----------------------------------------------
2919 '
2920 '条件xx停止受信
2921 '
2922 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2923 'fScewTCaseStop = 0　：正常終了
2924 '          　   　-1 ：異常終了
2925 '-----------------------------------------------
2926 Function M% fScewTCaseStop(ByVal MCase%())
2927 *ReCheckScewTCaseStop
2928     fScewTCaseStop = 0
2929     '条件xx停止を受信
2930     Wait M_In(MCase%(1)) = MOn%
2931     MRtn = fTimeOutJudge(MCase%(1),MOn%)
2932     If MRtn = 0 Then
2933         fScewTCaseStop = -1
2934     EndIf
2935     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
2936     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
2937     Dly 0.1
2938     '条件xx停止受信を送信
2939     M_Out(MCase%(2)) = MOn%
2940     Dly 0.5                          'とりあえず保持時間0.5msec
2941     'ねじ締め開始送信終了
2942     M_Out(MCase%(2)) = MOff%
2943 *ScewTCaseStop_ErrEnd
2944     '
2945 FEnd
2946 '
2947 '■fScrewTighenRoboCheck
2948 '<summary>
2949 'ねじロボ監視
2950 '</summary>
2951 '<param name = "MStopNum%"> 停止番号</param>
2952 '<returns>整数 0:ねじロボ異常終了 1:OK </returns>
2953 '<make>
2954 '2021/12/2 中村天哉
2955 '</make>
2956 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
2957     fnAutoScreenComment(503)    '状態表示[ねじロボ動作終了待ち] 2022/04/26 渡辺
2958     fScrewTighenRoboCheck = 1
2959     MScrewTighenRoboFlg% = 1    'フラグの初期化
2960     MCheck% = 0
2961     While MScrewTighenRoboFlg% = 1
2962         MCheck% = M_In16(11904)
2963         If M_In(MStopNum%) = 1 Then '停止位置まで来たら
2964             MScrewTighenRoboFlg% = 0 '関数を抜ける
2965             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
2966         EndIf
2967         If MCheck% <> 0 Then
2968             fScrewTighenRoboError(MCheck%)
2969             Select M_20#
2970                 Case MAbout%            '停止が押された場合
2971                     M_Out(12869) = 1 Dly 1.0
2972                     MScrewTighenRoboFlg% = 0
2973                     fScrewTighenRoboCheck = 0   '異常終了
2974                     Break
2975                 Case MNgProcess%        'NGが押された場合
2976                     M_Out(12873) = 1 Dly 1.0
2977                     MScrewTighenRoboFlg% = 0
2978                     fScrewTighenRoboCheck = 0   '異常終了
2979                     Break
2980                 Case MContinue%             'リトライが押された場合
2981                     M_20# = MClear%         'M_20#初期化
2982                     M_Out(12871) = 1 Dly 1.0
2983                     Break
2984                 Case MNext%                 '次へが押された場合
2985                     M_20# = MClear%         'M_20#初期化
2986                     M_Out(12874) = 1 Dly 1.0
2987                     Break
2988             End Select
2989             Dly 0.5
2990         EndIf
2991     WEnd
2992 FEnd
2993 '
2994 '■fScrewTighenRoboError
2995 '<summary>
2996 'ねじロボエラー処理
2997 '</summary>
2998 '<param name = "ErrorCode%"> エラー番号</param>
2999 '<make>
3000 '2021/12/2 中村天哉
3001 '</make>
3002 Function fScrewTighenRoboError(ByVal MErrorCode%)
3003     MErrorScreenCode% = 0
3004     MErrorScreenCode% = MErrorCode% + 300
3005     fErrorProcess(11,MErrorScreenCode%,0,0)
3006 FEnd
3007 '
3008 '■fErrorProcess
3009 '<summary>
3010 'エラー処理
3011 '</summary>
3012 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
3013 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
3014 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
3015 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
3016 '<make>
3017 '2021/11/5 中村天哉
3018 '</make>
3019 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3020     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
3021     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
3022     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
3023     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
3024 *RETRY_ERR_PROCESS
3025      M_20# = MClear%     '初期化
3026 '        'エラー処理記述
3027         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3028 '        'GOT KEY入力待ち
3029         MKeyNumber = fnKEY_WAIT()
3030 '        '
3031         If MKeyNumber = MAbout% Then   '停止を選択した場合
3032             M_20# = MAbout%            'M_20# プログラム間共通外部変数
3033             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3034             Break
3035          '
3036         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
3037             M_20# = MContinue%            'M_20# プログラム間共通外部変数
3038             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3039         '
3040         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
3041             M_20# = MNext%            'M_20# プログラム間共通外部変数
3042             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3043          '
3044         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
3045             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
3046             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3047             Break
3048         '
3049         EndIf
3050         '
3051         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3052 FEnd
3053 '
3054 '■fnTorqueCheck
3055 ''' <summary>
3056 ''' トルクチェック動作用のメイン
3057 ''' </summary>
3058 ''' <remarks>
3059 ''' Date   : 2021/12/21 : H.AJI
3060 ''' </remarks>'
3061 Function M% fnTorqueCheck
3062     'トルクチェック中送信  搬送系停止
3063     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
3064     '
3065     fnTorqueCheck = 0
3066     Ovrd 20
3067     Mov PInitialPosition              '初期位置移動
3068     Ovrd 100
3069     '下記キー待ちの継続に反応させないため
3070     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3071     Dly 0.2
3072     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3073     '
3074     'M6340  トルクチェック受信
3075     'Dly 5.0
3076     M_Out(12340) = 1          'トルクチェック受信 M6340
3077     Dly 1.0
3078     M_Out(12340) = 0
3079     '
3080     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3081     M_Out(12835) = 1                         'ねじロボトルクチェック画面切替
3082    Wait M_In(11843) = 1                         'ねじロボトルクチェック画面切替
3083     M_Out(12835) = 0                         'ねじロボトルクチェック画面切替完了
3084     '
3085     '
3086     MLoopFlg = 1
3087     While MLoopFlg = 1
3088         '
3089         Mov PInitialPosition              '初期位置移動
3090         '
3091         MKeyNumber = fnKEY_WAIT()
3092         Select MKeyNumber
3093             Case Is = 1           '停止
3094                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3095                 Dly 1.0
3096                 M_Out(12343) = 0
3097                 Ovrd 20
3098                 'Mov PTicketRead_1
3099                 M_Out(12840) = 1          'トルクチェック終了
3100                 Wait M_In(11859) = 1      'ねじロボからの終了
3101                 M_Out(12840) = 0          'トルクチェック終了
3102                 Ovrd 100
3103                 M_20# = 1
3104                 MLoopFlg = -1
3105                 Break
3106             Case Is = 2           '次へ
3107                 Break
3108             Case Is = 3           '継続
3109                 Break
3110             Case Is = 4           'トルクチェック開始
3111                 M_Out(12342) = 1          'トルクチェック開始要求受信 M6342
3112                 Dly 1.0
3113                 M_Out(12342) = 0
3114                 If M_In(11862) = 1 Then             'トルクチェッカー確認
3115                     fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3116                     MRet = fnScrewMTorque()           'ねじロボ用トルクチェック
3117                 EndIf
3118                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3119                 'MRet = fnMoveTorquePosi()
3120                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3121                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3122                 Break
3123             Default
3124                 Break
3125         End Select
3126     WEnd
3127     '
3128     'トルクチェック中停止送信
3129     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3130     '
3131     'ロボットの位置を元に戻す
3132     '
3133     '
3134  FEnd
3135  '
3136 '
3137 '
3138 '---------------------------
3139 '
3140 '    メイン画面の表示、非表示設定
3141 '         コメントD1001, D1002, D1003の設定
3142 '           MWindReSet = 0     画面非表示
3143 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3144 '           MWindErrScr = 10    エラー画面 D1001, D1002
3145 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3146 '
3147 '---------------------------
3148 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3149     fnMainScreenOpen = 0
3150     '
3151    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3152         M_Out16(12480) = MCommentD1001            'D1001 コメント
3153     EndIf
3154     '
3155     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3156         M_Out16(12496) = MCommentD1002            'D1002 コメント
3157     EndIf
3158     '
3159     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3160         M_Out16(12512) = MCommentD1003            'D1003 コメント
3161     EndIf
3162     '
3163     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3164     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3165     Dly 0.5
3166     M_Out(12362) = 0                         'ウィンド画面設定
3167 FEnd
3168 '
3169 '■Main
3170 ''' <summary>
3171 ''' トルクチェック実動作
3172 ''' </summary>
3173 ''' <remarks>
3174 ''' Date   : 2021/12/21 : H.AJI
3175 ''' </remarks>'
3176 Function M% fnScrewMTorque
3177     fnScrewMTorque = 0
3178     M_Out(12838) = 1                         'トルクチェック開始1
3179     Wait M_In(11857) = 1                     '受信完了
3180     M_Out(12838) = 0                         'トルクチェック開始1
3181     Dly 2.0
3182 FEnd
3183 '
3184 '
3185 '----------------------------------------------------------------
3186 'fTimeOutJudge
3187 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3188 '引数
3189 'Address% = 監視アドレス番号
3190 'JudgeFlg% = 対象アドレスの正常終了時の値
3191 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3192 '戻り値 = 0 エラー
3193 '         1 正常終了
3194 '         2 リトライ
3195 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3196 '作成日
3197 '2022/9/20 中村
3198 '----------------------------------------------------------------
3199 '
3200 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3201     fTimeOutJudge = 0
3202     MJudge% = 1
3203     MRtn = 0
3204     M_20# = MClear%
3205     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3206 *TimeOutLoop
3207     If MRtn = 1 Then GoTo *TimeOut
3208         fErrorProcess(11,202,203,0)
3209         If M_20# = MNext% Then GoTo *TimeOutLoop
3210         If M_20# = MContinue% Then MJudge% = 2
3211         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3212 *TimeOut
3213     fTimeOutJudge = MJudge%
3214 '
3215 *JUDGE_ERROR_END
3216 FEnd
3217 '■Main
3218 ''' <summary>
3219 ''' 組立動作用のメイン
3220 ''' </summary>
3221 ''' <remarks>
3222 ''' Date   : 2021/07/07 : M.Hayakawa
3223 ''' </remarks>'
3224 Function Main
3225     MopeNo = M_21#         '外部変数にて動作番号代入
3226     '
3227     If M_Svo=0 Then
3228         Servo On
3229     EndIf
3230     Wait M_Svo=1
3231 '組立スタート日付時刻要求パルスON
3232     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3233 'パトライト操作
3234     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3235     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3236     '
3237     M_20# = 0                                   'KEY入力初期化
3238     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
3239     MRet% = 0
3240 '初期位置の確認と移動
3241 '
3242 '復帰動作　実行・未実行判別      2022/04/08 渡辺 作成
3243     PActive = P_Curr                    '現在位置を取得
3244     MRecoveryPass% = 0
3245     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3246         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3247             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3248                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3249             EndIf
3250         EndIf
3251     EndIf
3252     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3253         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3254             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3255                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3256             EndIf
3257         EndIf
3258     EndIf
3259     If MRecoveryPass% = 0 Then
3260        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3261     EndIf
3262 '
3263     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3264         M_Out(12364) = 1            'toPLC_データ保存ON
3265 'トルクチェック
3266         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3267             MRet% = fnTorqueCheck()
3268             Break
3269         Else
3270 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
3271 '                MRtn = InspInit()               '画像処理初期化処理
3272 '            EndIf
3273 '
3274             M_20# = MClear%             '初期化
3275 '組立開始
3276             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3277                 fnAssyStart()
3278             Else
3279                 M_20# = MPass%
3280             EndIf
3281 '組立終了日付時刻
3282             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3283             Wait M_In(11572) = 1            '日付取得完了
3284             Dly 0.1
3285             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3286 'リフターユニットへのOUT
3287             '  KEY入力が何もない場合 OKと判断
3288             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3289             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3290 'OK/NGフラグ出力
3291             If M_20# <= 0 Then
3292                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3293             ElseIf M_20# = MPass% Then
3294                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3295             EndIf
3296 'PIASに組立完了書込み
3297             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3298                 If M_20# = MPass% Then
3299                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3300                 Else
3301                     'KEY入力がNGの場合
3302                     If M_20# = MNgProcess% Then
3303                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3304                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3305                         MRet% = fnPiasWrite(MNG%)
3306                        nAssyNgQty = nAssyNgQty + 1
3307                     EndIf
3308                     '
3309                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3310                     If M_20# = MAssyOK% Then
3311                             '-----------------------
3312                             'D732 -> D2600 コピー要求
3313                             M_Out(12566) = 1
3314 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3315                             M_Out(12566) = 0
3316                             '
3317                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3318                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3319                             '基板番号照合(PPは未使用）
3320 '                            MRet% = fnPCBNumberCheck()
3321                         Else
3322                             MRet% = 1
3323                         EndIf
3324                         '
3325                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3326                             If M_20# <> MAbout% Then
3327                                 '工程履歴OK書き込み
3328                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3329                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3330                                 MRet% = fnPiasWrite(MOK%)
3331                                 nAssyOkQty = 0
3332                                 nAssyOkQty = nAssyOkQty + 1
3333                             Else
3334                                 nAssyOkQty = nAssyOkQty + 1
3335                             EndIf
3336                         EndIf
3337                     EndIf
3338 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3339 '                    MRet% = fnPiasWrite(MOK%)
3340                 EndIf
3341             Else
3342                 nAssyOkQty = nAssyOkQty + 1
3343             EndIf
3344             '
3345             '組立終了日付時刻解除
3346             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3347             '投入数、組立OK数、組立NG数書込み
3348 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3349             '
3350 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3351 '                '画像処理終了処理
3352 '                MRtn = InspQuit()
3353 '            EndIf
3354         EndIf
3355         M_Out(12364) = 0                          'toPLC_データ保存OFF
3356     EndIf
3357 'パトライト操作
3358     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3359     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3360 'GOT表示
3361     fnAutoScreenComment(93)  'AUTO画面 工程完了
3362 FEnd
3363 End
3364 '
3365 'おまじないコメント
3366 '絶対削除するな
3367 '
3368 '
3369 '
3370 '
3371 '
3372 '
3373 '
PInspPosition(1)=(+602.00,-150.00,+500.00,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
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
PTemp=(+602.00,-150.00,+550.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
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
PActive=(+602.00,-150.00,+550.00,-180.00,+0.00,+90.00)(7,0)
Pmove=(-214.29,+565.14,+640.00,+179.97,-0.02,+4.00)(7,0)
PInitialPosition=(+340.00,+0.00,+580.00,-180.00,+0.00,+180.00)(7,0)
PMechaGet=(-418.66,-2.92,+305.03,+180.00,+0.00,-179.99)(7,1048577)
PMechaGet_1=(-418.66,-2.92,+410.00,+180.00,+0.00,-179.99)(7,1048577)
PMechaGet_2=(-189.84,-0.01,+629.06,-180.00,+0.00,-179.99)(7,1)
PMechaGet_3=(+0.01,+189.84,+629.07,-180.00,+0.00,+90.00)(7,0)
PMechaGet_4=(+327.50,+0.02,+596.24,-179.99,+0.00,+103.50)(7,0)
PMechaGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaSet1=(+167.10,-331.10,+318.57,-87.20,+88.18,-177.23)(6,0)
PMechaSet1_1=(+167.10,-331.10,+340.00,-87.20,+88.18,-177.23)(6,0)
PMechaSet2=(+169.45,-331.62,+319.06,-89.34,+88.05,-179.87)(6,0)
PMechaSet2_1=(+169.45,-331.62,+340.00,-89.34,+88.05,-179.87)(6,0)
PMechaSet_2=(+162.58,-305.37,+557.38,+179.47,+90.00,+89.47)(6,0)
PMechaSet_3=(+114.45,-288.22,+565.58,+180.00,+0.00,+112.11)(7,0)
PMechaSet_4=(+310.11,-0.04,+565.56,+180.00,+0.00,-179.55)(7,0)
PMechaSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck=(-90.21,+513.03,+577.72,-180.00,+0.00,-90.00)(7,0)
PPlateBackCheck_2=(+66.39,+429.86,+577.75,+180.00,+0.00,-90.00)(7,0)
PPlateBackCheck_3=(-18.78,+286.22,+630.88,+180.00,+0.00,-90.00)(7,0)
PPlateBackCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet=(+478.13,+103.51,+401.67,+179.65,+0.13,-179.14)(7,0)
PPlateBackGet_1=(+478.13,+103.51,+430.00,+179.65,+0.13,-179.14)(7,0)
PPlateBackGet_2=(+478.13,+103.51,+560.00,+179.65,+0.13,-179.14)(7,0)
PPlateBackGet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackPush=(-20.68,+418.34,+540.82,-180.00,+0.00,+90.51)(7,1048576)
PPlateBackPush_1=(-20.68,+400.00,+540.82,-180.00,+0.00,+90.51)(7,1048576)
PPlateBackPush_2=(-20.68,+380.00,+564.00,+180.00,+0.00,+90.00)(7,1048576)
PPlateBackSet=(-21.30,+459.99,+540.88,+179.43,-11.00,+90.80)(7,1048576)
PPlateBackSet_00=(-20.61,+498.13,+545.38,+179.77,+0.00,+90.43)(7,1048576)
PPlateBackSet_1=(-21.30,+452.59,+539.57,+179.40,-13.00,+90.81)(7,1048576)
PPlateBackSet_10=(-21.27,+354.31,+478.81,+179.08,-44.98,+90.81)(7,1048576)
PPlateBackSet_11=(-21.24,+351.47,+478.81,+179.08,-44.98,+90.91)(7,1048576)
PPlateBackSet_12=(-20.87,+345.00,+495.00,+179.08,-44.99,+90.43)(7,1048576)
PPlateBackSet_13=(-17.88,+286.22,+630.90,-179.82,-0.29,+90.49)(7,1048576)
PPlateBackSet_2=(-21.30,+439.30,+535.46,+179.35,-17.00,+90.82)(7,1048576)
PPlateBackSet_3=(-21.30,+425.40,+530.66,+179.29,-21.00,+90.84)(7,1048576)
PPlateBackSet_4=(-21.30,+413.50,+524.45,+179.22,-25.00,+90.87)(7,1048576)
PPlateBackSet_5=(-21.30,+399.71,+518.44,+179.15,-29.00,+90.90)(7,1048576)
PPlateBackSet_6=(-21.30,+388.51,+509.33,+179.08,-33.00,+90.94)(7,1048576)
PPlateBackSet_7=(-21.30,+377.51,+502.43,+179.00,-37.00,+90.98)(7,1048576)
PPlateBackSet_8=(-21.30,+366.41,+492.73,+178.90,-41.00,+91.04)(7,1048576)
PPlateBackSet_9=(-21.30,+356.07,+482.09,+179.08,-44.98,+91.11)(7,1048576)
PProductOnPltGet=(+479.40,-99.73,+372.85,+179.99,-0.35,-179.43)(7,0)
PProductOnPltGet_1=(+479.40,-99.73,+410.00,+179.99,-0.35,-179.43)(7,0)
PProductOnPltGet_2=(+479.40,-99.73,+510.00,+179.99,-0.35,-179.43)(7,0)
PProductOnPltGet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet=(+478.90,-99.73,+372.85,+179.99,-0.35,-179.43)(7,0)
PProductOnPltSet_1=(+478.90,-99.73,+410.00,+179.99,-0.35,-179.43)(7,0)
PProductOnPltSet_2=(+478.90,-99.73,+510.00,+179.99,-0.35,-179.43)(7,0)
PProductOnPltSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboGet=(-19.85,+405.99,+321.18,-111.55,+88.83,-21.44)(6,0)
PProductOnRoboGet_1=(-19.85,+405.99,+425.20,-112.91,+88.92,-23.00)(6,0)
PProductOnRoboGet_2=(-19.85,+387.42,+425.20,-112.91,+88.92,-22.80)(6,0)
PProductOnRoboGet_3=(-17.16,+300.00,+550.00,-66.19,+88.98,+23.82)(6,0)
PProductOnRoboGet_4=(-18.16,+300.00,+550.00,+175.04,+89.99,-94.95)(6,0)
PProductOnRoboGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboSet=(-19.85,+405.99,+321.18,-111.55,+88.83,-21.44)(6,0)
PProductOnRoboSet_1=(-19.85,+405.99,+425.20,-112.91,+88.92,-22.80)(6,0)
PProductOnRoboSet_2=(-19.85,+387.42,+425.20,-112.91,+88.92,-23.00)(6,0)
PProductOnRoboSet_3=(-17.16,+300.00,+550.00,-66.19,+88.98,+23.82)(6,0)
PProductOnRoboSet_4=(-18.86,+404.69,+360.00,-102.74,+89.08,-12.36)(6,0)
PProductOnRoboSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPushTilt=(-214.29,+565.14,+463.69,+179.97,-0.02,+4.00)(7,0)
PPushTilt_1=(-214.29,+565.14,+479.62,+179.97,-0.02,+4.00)(7,0)
PPushTilt_2=(-214.29,+565.14,+620.00,+179.97,-0.02,+4.00)(7,0)
PPushTilt_3=(+0.02,+340.00,+610.00,-180.00,-0.01,-91.91)(7,0)
PTicketRead=(+602.00,-150.00,+500.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_1=(+602.00,-150.00,+550.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
JActive=(+110.77,+45.12,+39.40,-0.01,+95.51,-73.23)
Jmove=(+110.77,-46.87,+111.64,+0.00,+80.58,-73.23,+0.00,+0.00)
JTaihi=(+0.00,-46.87,+111.64,+0.00,+80.58,+0.00,+0.00,+0.00)
