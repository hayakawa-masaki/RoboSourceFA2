1 ' ===================================
2 '
3 '  210512003 STEP5 Assy3プログラム
4 '
5 ' 作成者：自動化T
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
213 Def Inte MSuctionErrQty     '吸着エラー数 演算変数 2022/04/27 渡辺
214 Def Inte nAssyOkQty         '未使用
215 Def Inte MScrewNo
216 Def Inte MReTry
217 '===== <IO変数定義> =====
218 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
219 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
220 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
221 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
222 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
223 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
224 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
225 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
226 '
227 Def Inte Y68_VV1            ' アーム先端　ネジ吸着バルブ
228 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
229 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
230 '
231 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
232 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
233 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
234 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
235 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
236 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
237 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
238 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
239 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
240 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
241 '
242 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
243 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
244 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
245 '
246 Def Inte MOUT_LED1          ' 画像処理用LED照明
247 '
248 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
249 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
250 '
251 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
252 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
253 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
254 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
255 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
256 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
257 '
258 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
259 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
260 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
261 '
262 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
263 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
264 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
265 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
266 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
267 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
268 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
269 Y68_VV1%    =  12248    ' アーム先端　ネジ吸着バルブ '数値12250から12248へ変更(8/5中村)
270 Y6B_VB1%    =  12250    'アーム先端　吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
271 MOUT_VB1%   =  12250    ' アーム先端　ネジ吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
272 '
273 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
274 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
275 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
276 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
277 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
278 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
279 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
280 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
281 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
282 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
283 '
284 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
285 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
286 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
287 '
288 MOUT_LED1%  =  12239    ' 画像処理用LED照明
289 '
290 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
291 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
292 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
293 '
294 '共通
295 Def Inte MTEST_KEY                      'デバックテスト用
296 Def Inte MOn                            '出力=1
297 Def Inte MOff                           '出力=0
298 '
299 'ねじ締め装置_出力アドレス
300 Def Inte MOUT_ScwT_ComChk               '通信確認
301 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
302 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
303 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
304 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
305 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
306 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
307 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
308 'ねじ締め装置_入力アドレス
309 Def Inte MIN_ScwT_comOK                 '通信確認返信
310 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
311 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
312 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
313 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
314 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
315 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
316 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
317 '
318 Dim MScwT_Case1%(2)               '条件1停止変数
319 Dim MScwT_Case2%(2)               '条件2停止変数
320 Dim MScwT_Case3%(2)               '条件3停止変数
321 Dim MScwT_Case4%(2)               '条件4停止変数
322 Dim MScwT_Case5%(2)               '条件5停止変数
323 '
324 '共通
325 MTEST_KEY% = 11359                       'デバッグ用テストKEY
326 MOn% = 1                                 '出力 = 1
327 MOff% = 0                                '出力 = 0
328 '
329 'ねじ締め機_アドレス設定
330 MOUT_ScwT_ComChk% = 12832               '通信確認送信
331 MOUT_ScwT_ST% = 12865                   'ねじ締め開始を送信
332 MOUT_ScwT_ReSTOK% = 12866               '再開始受信を送信
333 MOUT_ScwT_FinOK% = 12868                'ねじ締め完了受信を送信
334 MOUT_ScwT_Case1OK% = 12874              '条件1停止受信を送信
335 MOUT_ScwT_Case2OK% = 12875              '条件2停止受信を送信
336 MOUT_ScwT_Case3OK% = 12876              '条件3停止受信を送信
337 MOUT_ScwT_Case4OK% = 12877              '条件4停止受信を送信
338 MOUT_ScwT_Case5OK% = 12878              '条件5停止受信を送信
339 '
340 MIN_ScwT_comOK% = 11840                 'ねじ締め装置から返信
341 MIN_ScwT_STRec% = 11873                 'ねじ締め開始を受信
342 MIN_ScwT_ReST% = 11874                  '再開始を受信
343 MIN_ScwT_Fin% = 11876                   'ねじ締め完了を受信
344 MIN_ScwT_Case1% = 11882                 '条件1停止待機を受信
345 MIN_ScwT_Case2% = 11883                 '条件2停止待機を受信
346 MIN_ScwT_Case3% = 11884                 '条件3停止待機を受信
347 MIN_ScwT_Case4% = 11885                 '条件4停止待機を受信
348 MIN_ScwT_Case5% = 11886                 '条件5停止待機を受信
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
361 '設定 InitialZoneBで使用する変数
362 Def Pos PActive       '直交座標系 位置変数 現在位置
363 Def Pos Pmove         '直交座標系 位置変数 移動先
364 Def Jnt JActive       '関節座標系 位置変数 現在位置
365 Def Jnt Jmove         '関節座標系 位置変数 移動先
366 Def Jnt JTaihi        '関節座標系 位置変数 退避ポジション ティーチングで設定
367 Def Inte MRecoveryPass      '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行
368 Def Inte MStandby              '待機位置確認フラグ
369 Def Inte MRecoveryChuckOpen    'チャック解放フラグ（復帰動作前）両掴み対策
370 '★注意★初期位置を変更した時には、変更が必要！
371 '
372 'エラー対策
373 Def Inte MCtlNo    'Functionエラー対策 2022/04/15 渡辺
374 '
375 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
376 Function M% fnAssyStart
377     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/27 渡辺
378     M_20# = MClear%                       '初期化
379 ''    'ねじ締め開始(処理位置変更2/16中村)
380 '    fScewTStart()
381 '
382 ' 組立開始
383 'プログラム原点            '(追加ここから(8/30中村))
384 Ovrd 100
385 '
386 'ハンドに本体,側板がないか確認
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
406 'ハンドをイニシャルに戻す
407 If M_In(11266) = 1 Then             '本体チャック閉検出
408     M_Out(12256) = 0                '本体チャック閉OFF
409     M_Out(12257) = 1                '本体チャック開ON
410     Break
411 EndIf
412 If M_In(11269) = 1 Then             '側板Lシリンダー出検出
413     M_Out(12258) = 0                '側板Lシリンダー出OFF
414     M_Out(12259) = 1                '側板Lシリンダー戻ON
415     Break
416 EndIf
417 If M_In(11274) = 1 Then             '側板Rシリンダー出検出
418     M_Out(12260) = 0                '側板Rシリンダー出OFF
419     M_Out(12261) = 1                '側板Rシリンダー戻ON
420     Break
421 EndIf
422 M_Out(12262) = 0                    '側板チャック閉OFF
423 M_Out(12263) = 1                    '側板チャック開ON
424 '
425 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)'本体チャック開検出
426 If MRtn = 1 Then GoTo *CompInitial_3
427 fErrorProcess(11,244,281,0)
428 If M_20# = MNext% Then M_20# = MClear%
429 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
430 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
431 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
432 *CompInitial_3
433 '
434 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)'側板L戻検出
435 If MRtn = 1 Then GoTo *CompInitial_4
436 fErrorProcess(11,247,281,0)
437 If M_20# = MNext% Then M_20# = MClear%
438 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
439 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
440 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
441 *CompInitial_4
442 '
443 MRtn = frInCheck(11273,1,MSETTIMEOUT05&)'側板R戻検出
444 If MRtn = 1 Then GoTo *CompInitial_5
445 fErrorProcess(11,249,281,0)
446 If M_20# = MNext% Then M_20# = MClear%
447 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
448 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
449 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
450 *CompInitial_5
451 '
452 '初期位置を設定
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
466 '    M_Out(12265) = 0            '位置決め戻OFF
467 '    M_Out(12264) = 1            '位置決め出ON
468 '    Mov PTicketRead
469 '    Break
470 'Else
471 '    Mov PInitialPosition
472 '    M_Out(12265) = 0            '位置決め戻OFF
473 '    M_Out(12264) = 1            '位置決め出ON
474 '    Mov PTicketRead_1           'チケットID読み取り回避点
475 '    Mvs PTicketRead             'ID読み位置
476 '    Break
477 'EndIf
478 '
479 ' 2022/04/12 安全方向へ条件変更 渡辺
480 ' PInitialPosition 在席 MStandby=2
481 ' PTicketRead_1 在席 MStandby=1
482 '
483 MStandby = 0    '待機位置フラグを初期化
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
499     M_Out(12265) = 0            '位置決め戻OFF
500     M_Out(12264) = 1            '位置決め出ON
501     Mov PTicketRead_1           'チケットID読み取り回避点
502     Mvs PTicketRead             'ID読み位置
503     Break
504 EndIf
505 If MStandby = 1 Then
506     M_Out(12265) = 0            '位置決め戻OFF
507     M_Out(12264) = 1            '位置決め出ON
508     Mvs PTicketRead             'ID読み位置
509     Break
510 EndIf
511 If MStandby <> 0 Then GoTo *PositionOK
512 fErrorProcess(11,230,281,0)           '初期位置にいない時はエラーにする
513 If M_20# = MNext% Then GoTo *ASSY_ERROR_END
514 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
515 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
516 If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
517 *PositionOK
518 '
519 MRtn = 1        'MRtn初期化
520 'チケットIDを読む
521 *RE_TICKET_READ
522 M_20# = MClear%                       '初期化
523 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
524     MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
525     '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
526     '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
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
539 Mov PTicketRead_1               'チケットID読み取り回避点
540 'Dly 10                   'デバッグ用(22/09/30中村)
541 '    'ねじ締め開始(処理位置変更2/16中村)
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
553 'パレットから製品を取る
554 Mov PProductOnPltGet_2      '本体回避点
555 '
556 *RE_PLT_GET_1
557 '
558 If M_20# = MContinue% Then M_20# = MClear%
559 '
560 M_Out(12256) = 0            '本体チャック閉OFF
561 M_Out(12257) = 1            '本体チャック開ON
562 '
563 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)        '本体チャック開センサーON
564 If MRtn = 1 Then GoTo *CompPltGet_1
565 fErrorProcess(11,244,284,0)
566 If M_20# = MNext% Then M_20# = MClear%
567 If M_20# = MAbout% Or M_20# = MNgProcess% Then
568     Mov PInitialPosition
569     M_Out(12264) = 0            '位置決め出OFF
570     M_Out(12265) = 1            '位置決め戻ON
571     Break
572 EndIf
573 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
574 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
575 If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
576 *CompPltGet_1
577 '
578 'Mov PProductOnPltGet_1      '本体上空(処理位置変更1/21中村)
579 'Wait M_In(11278) = 1        '位置決め出端検出(コメントアウト11/4中村)
580 MRtn = frInCheck(11278,1,MSETTIMEOUT05&)        '位置決め出端検出
581 If MRtn = 1 Then GoTo *CompPltGet_2
582 fErrorProcess(11,231,282,0)
583 If M_20# = MNext% Then M_20# = MClear%
584 If M_20# = MAbout% Or M_20# = MNgProcess% Then
585     Mov PProductOnPltGet_2
586     Mov PInitialPosition
587     M_Out(12264) = 0            '位置決め出OFF
588     M_Out(12265) = 1            '位置決め戻ON
589     Break
590 EndIf
591 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
592 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
593 If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
594 *CompPltGet_2
595 '
596 Mov PProductOnPltGet_1      '本体上空(処理位置変更1/21中村)
597 '
598 M_Out(12264) = 0            '位置決め出OFF
599 M_Out(12265) = 1            '位置決め戻ON
600 Ovrd 25
601 Mvs PProductOnPltGet        '本体を取る位置
602 '
603 *RE_PLT_GET_2
604 '
605 If M_20# = MContinue% Then M_20# = MClear%
606 '位置決め戻のエラー処理
607 MRtn = frInCheck(11279,1,MSETTIMEOUT05&)    '位置決め戻端検出
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
618     M_Out(12265) = 0            '位置決め戻OFF
619     M_Out(12264) = 1            '位置決め出ON
620     Dly 1.0
621     M_Out(12264) = 0                            '位置決め出OFF
622     M_Out(12265) = 1                            '位置決め戻ON
623 EndIf
624 If M_20# = MContinue% Then GoTo *RE_PLT_GET_2
625 *CompPushIni
626 '
627 M_Out(12257) = 0            '本体チャック開OFF
628 M_Out(12256) = 1            '本体チャック閉ON
629 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '本体チャック閉センサーON
630 If MRtn = 1 Then GoTo *CompPltGet_3
631 M_Out(12256) = 0            '本体チャック閉OFF
632 M_Out(12257) = 1            '本体チャック開ON
633 Dly 2.0
634 Mvs PProductOnPltGet_1
635 Mov PProductOnPltGet_2
636 M_Out(12257) = 0            '本体チャック開OFF
637 M_Out(12256) = 1            '本体チャック閉ON
638 fErrorProcess(11,244,284,0)
639 If M_20# = MNext% Then M_20# = MClear%
640 If M_20# = MAbout% Or M_20# = MNgProcess% Then
641     Mov PInitialPosition
642     Break
643 EndIf
644 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
645 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
646 M_Out(12256) = 0            '本体チャック閉OFF
647 M_Out(12257) = 1            '本体チャック開ON
648 Dly 2.0
649 Mov PProductOnPltGet_1
650 Mvs PProductOnPltGet
651 If M_20# = MContinue% Then GoTo *RE_PLT_GET_2
652 M_Out(12257) = 0            '本体チャック開OFF
653 M_Out(12256) = 1            '本体チャック閉ON
654 Dly 2.0
655 *CompPltGet_3
656 '
657 MRth = frInCheck(11264,1,MSETTIMEOUT05&)        '本体検出センサーON
658 If MRtn = 1 Then GoTo *CompPltGet_4
659 M_Out(12256) = 0            '本体チャック閉OFF
660 M_Out(12257) = 1            '本体チャック開ON
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
675 M_Out(12257) = 0            '本体チャック開OFF
676 M_Out(12256) = 1            '本体チャック閉ON
677 Dly 2.0
678 *CompPltGet_4
679 '
680     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
681 Mvs PProductOnPltGet_1          '本体上空
682     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
683 '
684 'Ovrd 100
685 Moverride = 2000 / M_OPovrd
686 If Moverride > 100 Then Moverride = 100
687 Ovrd Moverride
688 '
689 Mov PProductOnPltGet_2      '本体回避点
690 '
691 '製品をねじロボ1に置く
692 Mov PProductOnRoboSet_2     'ねじロボ1回避点
693 'Wait M_In(11888) = 1        'ねじロボ1停止1受信
694 MScrewRoboNgFlg% = 0
695 '
696 MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
697 '
698 If MRtn = 0 Then MScrewRoboNgFlg% = 1
699 If MScrewRoboNgFlg% = 1 Then GoTo *ProductOnPltSet
700 Mov PProductOnRoboSet_1     'ねじロボ1上空
701 Ovrd 25
702 Mvs PProductOnRoboSet       '本体置き位置
703 *RE_ROBO_SET
704 If M_20# = MContinue% Then M_20# = MClear%
705 '
706 Dly 0.3
707 M_Out(12256) = 0            '本体チャック閉OFF
708 M_Out(12257) = 1            '本体チャック開ON
709 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)             '本体チャック開センサーON
710 If MRtn = 1 Then GoTo *CompRoboSet
711 fErrorProcess(11,244,284,0)
712 If M_20# = MNext% Then M_20# = MClear%
713 If M_20# = MAbout% Or M_20# = MNgProcess% Then
714     MScrewRoboNgFlg% = 1
715     Mvs PProductOnRoboSet_1
716     Mov PProductOnRoboSet_2
717     Break
718 EndIf
719 If M_20# = MAbout% Then GoTo *ProductOnPltSet    '念のため製品を置きに行く動作を行う
720 If M_20# = MNgProcess% Then GoTo *ProductOnPltSet    '念のため製品を置きに行く動作を行う
721 If M_20# = MContinue% Then GoTo *RE_ROBO_SET
722 *CompRoboSet
723 '
724 Mvs PProductOnRoboSet_1     'ねじロボ1上空
725 Ovrd 100
726 Mvs PProductOnRoboSet_2     'ねじロボ1回避点
727 M_Out(12866) = 1 Dly 0.5    'ねじロボ1動作再開(停止1～停止2)(処理位置変更1/21中村)
728 '
729 '
730 '側板Lをパレットから取る
731 Mov PPlateLGet_2            '側板L取り回避点
732 'M_Out(12866) = 1 Dly 0.5    'ねじロボ1動作再開(停止1～停止2)(処理位置変更1/21中村)
733 *RE_PLATE_L_GET_1
734 If M_20# = MContinue% Then M_20# = MClear%
735 '
736 M_Out(12257) = 0            '本体チャック開OFF(以下3行,側板受け取り時邪魔になるため)
737 M_Out(12256) = 1            '本体チャック閉ON
738 '
739 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '本体チャック閉センサーON
740 If MRtn = 1 Then GoTo *CompPlateLGet_1
741 fErrorProcess(11,245,284,0)
742 If M_20# = MNext% Then M_20# = MClear%
743 If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
744 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
745 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
746 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_1
747 *CompPlateLGet_1
748 '
749 Mov PPlateLGet_1            '側板L取り上空
750 Ovrd 10
751 M_Out(12259) = 0            '側板Lシリンダー戻OFF
752 M_Out(12258) = 1            '側板Lシリンダー出ON
753 MRtn = frInCheck(11269,1,MSETTIMEOUT05&)        '側板Lシリンダー出端検出センサーON
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
769 M_Out(12262) = 0            '側板チャック閉OFF
770 M_Out(12263) = 1            '側板チャック開ON
771 Fine 0.05 , P               '公差0.05[mm]以内
772 Mvs PPlateLGet              '側板Lを取る位置
773 Fine 0 , P                  'Fine解除
774 M_Out(12263) = 0            '側板チャック開OFF
775 M_Out(12262) = 1            '側板チャック閉ON
776 'Wait M_In(11271) = 1        '側板チャック閉検出センサーON
777 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)        '側板Lチャック閉検出センサーON
778 Dly 0.5
779 Mvs PPlateLGet_1            '側板L取り上空
780 If MRtn = 1 Then GoTo *CompPlateLGet_3
781 fErrorProcess(11,250,292,0) '284→292に変更(6/7中村)
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
792 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)         '側板L検出
793 If MRtn = 1 Then GoTo *CompPlateLGet_4
794 fErrorProcess(11,254,292,0) '284→292に変更(6/7中村)
795 If M_20# = MNext% Then M_20# = MClear%
796 If M_20# = MAbout% Or M_20# = MNgProcess% Then
797     Mov PPlateLGet_2
798     Mov PInitialPosition
799 EndIf
800 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
801 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
802 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_2
803 *CompPlateLGet_4
804 'Dly 5                      'test(暫定コメントアウト)
805 'M_Out(12256) = 0            '本体チャック閉OFF(側板受け取り時邪魔になるため)以下3行暫定削除(11/17中村)
806 'M_Out(12257) = 1            '本体チャック開ON(側板受け取り時邪魔になるため)
807 'MRtn = frInCheck(11265,1,MSETTIMEOUT05&)         '本体チャック開センサーON
808 'If MRtn = 0 Then
809 '    fErrorProcess()         'エラー処理
810 'EndIf
811 'Ovrd 100
812 Mov PPlateLGet_2            '側板L取り回避点
813 Ovrd 100
814 '
815 '側板Lを置く
816 Mov PPlateLSet_2            '側板L置き回避点
817 '
818 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)         'もう一度側板L検出
819 If MRtn = 1 Then GoTo *CompPlateLGet_5
820 fErrorProcess(11,254,292,0) '284→292に変更(6/7中村)
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
834 'Wait M_In(11889) = 1        'ねじロボ1停止2受信
835 MRtn = fScrewTighenRoboCheck(11889)    '停止状態を受信する
836 If MRtn = 0 Then Mov PInitialPosition
837 If MRtn = 0 Then GoTo *ASSY_ERROR_END
838 '
839 'Wait M_In(11889) = 1        'ねじロボ1停止2受信
840 MRtn = fScrewTighenRoboCheck(11889)    '停止状態を受信する
841 If MRtn = 0 Then Mov PInitialPosition
842 If MRtn = 0 Then GoTo *ASSY_ERROR_END
843 '
844 Mov PPlateLSet_1            '側板L置き上空
845 Ovrd 10
846 Mvs PPlateLSet              '側板Lを置く位置
847 Dly 0.2
848 M_Out(12866) = 1 Dly 0.5    'ねじロボ1動作再開(停止2～停止3)
849 'Wait M_In(11890) = 1        'ねじロボ1停止3受信
850 MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する
851 'If MRtn = 0 Then
852 '    Mvs PPlateLSet_1
853 '    Mov PPlateLSet_2
854 '    Mov PInitialPosition
855 'EndIf
856 If MRtn = 0 Then GoTo *ASSY_ERROR_END
857 M_Out(12262) = 0            '側板チャック閉OFF
858 M_Out(12263) = 1            '側板チャック開ON
859 Dly 0.5
860 Mvs PPlateLSet_1            '側板L置き上空
861 Ovrd 100
862 M_Out(12258) = 0            '側板Lシリンダー出OFF
863 M_Out(12259) = 1            '側板Lシリンダー戻ON
864 Mov PPlateLSet_2            '側板L置き回避点
865 '
866 '    ' 部品供給要求送信'12/20位置変更(中村)
867     M_Out(12787) = 1
868 '
869 '側板L置き位置画像検査
870 Mov PPlateLCheck_2          '側板L検査通過点
871 Mvs PPlateLCheck            '側板L検査位置
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
887 M_Out(12866) = 1 Dly 0.5    'ねじロボ動作再開(停止3～停止4)
888 '
889 '側板Rを供給機から取る
890 '
891 '    ' 部品供給要求送信'12/20位置変更(中村)
892 '    M_Out(12787) = 1
893 '    '    ' 部品供給完了待ち
894 '    Wait M_In(11810) = 1
895 '
896 Mov PPlateRGet_4            '経路1
897 Mov PPlateRGet_3            '経路2
898 Mov PPlateRGet_2            '側板R取り回避点
899     '    ' 部品供給完了待ち(処理変更2/27中村)
900 *RE_FEEDER_READY
901     fnAutoScreenComment(513)    '状態表示[部品供給待ち] 2022/04/27 渡辺
902 '    Wait M_In(11810) = 1
903 MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '供給待ち
904 If MRtn = 1 Then GoTo *CompFeederReady
905 '   ' 部品供給要求終了
906 M_Out(12787) = 0
907 fErrorProcess(11,289,290,0) '284→290に変更6/7中村
908 If M_20# = MNext% Then M_20# = MClear%
909 If M_20# = MAbout% Or M_20# = MNgProcess% Then
910     Mov PBracketRGet_2
911     Mov PBracketRGet_3
912     Mov PBracketRSet_3
913     Mov PInitialPosition1
914 EndIf
915 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
916 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
917     ' 部品供給要求
918 M_Out(12787) = 1
919 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
920 *CompFeederReady
921 '    ' 部品供給要求終了
922     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/27 渡辺
923     M_Out(12787) = 0
924 '
925 *RE_PLATE_R_GET_1
926 '
927 If M_20# = MContinue% Then M_20# = MClear%
928 '
929 M_Out(12257) = 0            '本体チャック開OFF(以下3行,側板受け取り時邪魔になるため)
930 M_Out(12256) = 1            '本体チャック閉ON
931 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '本体チャック閉センサーON
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
951 Mov PPlateRGet_1            '側板R取り上空
952 Ovrd 10
953 M_Out(12261) = 0            '側板Rシリンダー戻OFF
954 M_Out(12260) = 1            '側板Rシリンダー出ON
955 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)        '側板Rシリンダー出端検出センサーON
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
971 M_Out(12262) = 0            '側板チャック閉OFF
972 M_Out(12263) = 1            '側板チャック開ON
973 Fine 0.05 , P               '公差0.05[mm]以内
974 Mvs PPlateRGet              '側板Rを取る位置
975 Fine 0 , P                  'Fine解除
976 M_Out(12263) = 0            '側板チャック開OFF
977 M_Out(12262) = 1            '側板チャック閉ON
978 Dly 0.5
979 Mvs PPlateRGet_1            '側板R取り上空
980 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '側板R検出
981 If MRtn = 1 Then GoTo *CompPlateRGet_3
982 fErrorProcess(11,254,292,0) '284→292に変更(6/7中村)
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
996 M_Out(12256) = 0            '本体チャック閉OFF(側板受け取り時邪魔になるため)
997 M_Out(12257) = 1            '本体チャック開ON(側板受け取り時邪魔になるため)
998 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)        '本体チャック開センサーON
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
1014 Mov PPlateRGet_2            '側板R取り回避点
1015 Ovrd 100
1016 Mov PPlateRGet_3            '経路2
1017 ''    ' 部品供給要求終了
1018 '    M_Out(12787) = 0
1019 ''    ' 部品取得完了送信(パルス)
1020 '    M_Out(12800) = 1 Dly 0.5
1021     '
1022 Mov PPlateRGet_4            '経路1
1023 '
1024 '側板Rを置く
1025 Mov PPlateRSet_3            '経路
1026 Mov PPlateRSet_2            '側板R置き回避点
1027 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        'もう一度側板R検出
1028 If MRtn = 1 Then GoTo *CompRGet
1029 fErrorProcess(11,254,292,0) '284→292に変更(6/7中村)
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
1050 '    ' 部品供給要求終了
1051     M_Out(12787) = 0
1052 '    ' 部品取得完了送信(パルス)
1053     M_Out(12800) = 1 Dly 0.5
1054 'Wait M_In(11891) = 1        'ねじロボ1停止4受信
1055 MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
1056 If MRtn = 0 Then Mov PInitialPosition
1057 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1058 Mov PPlateRSet_1            '側板R置き上空
1059 Ovrd 10
1060 Mvs PPlateRSet              '側板Rを置く位置
1061 Dly 0.2
1062 M_Out(12866) = 1 Dly 0.5    'ねじロボ動作再開(停止4～停止5)
1063 'Wait M_In(11892) = 1        'ねじロボ1停止5受信
1064 MRtn = fScrewTighenRoboCheck(11892)    '停止状態を受信する
1065 'If MRtn = 0 Then
1066 '    Mvs PPlateRSet_1
1067 '    Mov PPlateRSet_2
1068 '    Mov PInitialPosition
1069 'EndIf
1070 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1071 M_Out(12262) = 0            '側板チャック閉OFF
1072 M_Out(12263) = 1            '側板チャック開ON
1073 Dly 0.5
1074 Mvs PPlateRSet_1            '側板L置き上空
1075 Ovrd 100
1076 M_Out(12260) = 0            '側板Rシリンダー出OFF
1077 M_Out(12261) = 1            '側板Rシリンダー戻ON
1078 Mov PPlateRSet_2            '側板R置き回避点
1079 '
1080 '側板R置き位置画像検査
1081 Mov PPlateRCheck_2          '側板R検査通過点
1082 Mvs PPlateRCheck            '側板R検査位置
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
1098 M_Out(12866) = 1 Dly 0.5    'ねじロボ動作再開(停止5～停止6)
1099 '
1100 'ねじロボ1の製品を取る
1101 Mov PProductOnRoboGet_2     'ねじロボ1回避点
1102 'Wait M_In(11893) = 1        'ねじロボ1停止6受信
1103 'MRtn = frInCheck(11876,1,MSETTIMEOUT05&)   'ねじロボ1完了を受信
1104 '
1105 *RE_CYLINDER_R_INI
1106 MRtn = frInCheck(11273,1,MSETTIMEOUT05&)        'R側シリンダー戻検出
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
1120     M_Out(12260) = 0            '側板Rシリンダー出OFF
1121     M_Out(12261) = 1            '側板Rシリンダー戻ON
1122 EndIf
1123 If M_20# = MContinue% Then GoTo *RE_CYLINDER_R_INI
1124 *CompCylinderRIni
1125 '
1126 MRtn = fScrewTighenRoboCheck(11893)    '停止状態を受信する
1127 If MRtn = 0 Then Mov PInitialPosition
1128 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1129 'Mvs PProductOnRoboGet_1     'ねじロボ1上空(2022/1/11移動タイミング変更(中村))
1130 'Ovrd 25
1131 '
1132 *RE_ROBO_GET
1133 '
1134 If M_20# = MContinue% Then M_20# = MClear%
1135 '
1136 M_Out(12256) = 0            '本体チャック閉OFF
1137 M_Out(12257) = 1            '本体チャック開ON
1138 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)        '本体チャック開センサーON
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
1152 Mvs PProductOnRoboGet_1     'ねじロボ1上空(2022/1/11移動タイミング変更(中村))
1153 Ovrd 25
1154 '
1155 Mvs PProductOnRoboGet       '本体を取る位置
1156 M_Out(12866) = 1 Dly 0.5    'ねじロボ動作再開(停止6～完了)
1157 'Wait M_In(11876) = 1        'ねじロボ1完了を受信
1158 MRtn = fScrewTighenRoboCheck(11876)    '停止状態を受信する
1159 If MRtn = 0 Then
1160     Mvs PProductOnRoboGet_1
1161     Mvs PProductOnRoboGet_2
1162     Mov PInitialPosition
1163 EndIf
1164 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1165 '
1166 M_Out(12257) = 0            '本体チャック開OFF
1167 M_Out(12256) = 1            '本体チャック閉ON
1168 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '本体チャック閉センサーON
1169 If MRtn = 1 Then GoTo *CompRoboGet_2
1170 M_Out(12256) = 0            '本体チャック閉OFF
1171 M_Out(12257) = 1            '本体チャック開ON
1172 Dly 2.0
1173 Mvs PProductOnRoboGet_1
1174 Mvs PProductOnRoboGet_2
1175 Mov PInitialPosition
1176 M_Out(12257) = 0            '本体チャック開OFF
1177 M_Out(12256) = 1            '本体チャック閉ON
1178 fErrorProcess(11,245,284,0)
1179 If M_20# = MNext% Then M_20# = MClear%
1180 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1181 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1182 M_Out(12256) = 0            '本体チャック閉OFF
1183 M_Out(12257) = 1            '本体チャック開ON
1184 Dly 2.0
1185 Mov PProductOnRoboGet_2
1186 If M_20# = MContinue% Then GoTo *RE_ROBO_GET
1187 Mvs PProductOnRoboGet_1
1188 Mvs PProductOnRoboGet
1189 M_Out(12257) = 0            '本体チャック開OFF
1190 M_Out(12256) = 1            '本体チャック閉ON
1191 Dly 2.0
1192 *CompRoboGet_2
1193 '
1194 MRtn = frInCheck(11264,1,MSETTIMEOUT05&)        '本体検出センサーON
1195 'Mvs PProductOnRoboGet_1     'ねじロボ1上空
1196 If MRtn = 1 Then GoTo *CompRoboGet_3
1197 M_Out(12256) = 0            '本体チャック閉OFF
1198 M_Out(12257) = 1            '本体チャック開ON
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
1211 M_Out(12257) = 0            '本体チャック開OFF
1212 M_Out(12256) = 1            '本体チャック閉ON
1213 Dly 2.0
1214 *CompRoboGet_3
1215 '
1216 Moverride = 2000 / M_OPovrd
1217 If Moverride >100 Then Moverride = 100
1218 Ovrd Moverride
1219 Mov PProductOnRoboGet_2     'ねじロボ1回避点
1220 '
1221 *ProductOnPltSet
1222 'パレットに製品を置く
1223 Mov PProductOnPltSet_2     'パレット回避点
1224 Mov PProductOnPltSet_1     'パレット上空
1225 Ovrd 10
1226 Mvs PProductOnPltSet       '本体置き位置
1227 Dly 0.2
1228 '
1229 *RE_PLT_SET
1230 '
1231 If MScrewRoboNgFlg% = 1 And M_20# = MContinue% Then M_20# = MRtn
1232 If M_20# = MContinue% Then M_20# = MClear%
1233 '
1234 M_Out(12256) = 0            '本体チャック閉OFF
1235 M_Out(12257) = 1            '本体チャック開ON
1236 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    '本体チャック開センサーON
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
1250 Mvs PProductOnPltSet_1     'パレット上空
1251 Ovrd 100
1252 Mvs PProductOnPltSet_2     'パレット回避点
1253 '
1254 'Mov PInitialPosition        '暫定削除(11/17中村)
1255     MRtn = FnCtlValue2(2)          '組立ＯＫ＋１  2022/04/28 渡辺
1256 Mov PTicketRead_1
1257     MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1258 If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
1259 M_Out(12868) = 1 Dly 0.5    'ねじロボ1ねじ締め完了を送信
1260 '
1261 M_20# = MAssyOK%            'Assy正常終了
1262 '
1263 GoTo *AssyEnd
1264 '
1265 *ASSY_ERROR_END
1266     M_Out(12264) = 0            '位置決め出OFF
1267     M_Out(12265) = 1            '位置決め戻ON
1268 *AssyEnd
1269 *fnAssyStart_FEndPosi
1270     Exit Function
1271 FEnd
1272 '
1273 '■fnPiasCheck
1274 ''' <summary>
1275 ''' PIASチケット読込み
1276 ''' </summary>
1277 ''' <returns>   0 : NG
1278 '''             1 : OK(読込み完了)
1279 ''' </returns>
1280 ''' <remarks>
1281 ''' Date   : 2021/07/07 : M.Hayakawa
1282 ''' </remarks>'
1283 Function M% fnPiasCheck
1284     fnPiasCheck = 0
1285     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1286     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1287 '
1288 *RETRY_PIAS
1289     M_20# = MClear%
1290     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1291     '
1292     '【IDチケット読み込み】
1293     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1294     MInspGroup%(1) = 1              '検査G番号
1295     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1296 '
1297     'エラーの場合
1298     If MRtn <> 1 Then
1299         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1300         If MRtn <> 1 Then
1301             'D720 -> D1300 コピー要求
1302             M_Out(12565) = 1
1303             Dly 0.5
1304             M_Out(12565) = 0
1305             'エラー処理記述
1306             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1307             'GOT KEY入力待ち
1308             MKeyNumber = fnKEY_WAIT()
1309             '
1310             Select MKeyNumber
1311                 Case MNext%         '次へを選択した場合
1312                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1313                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1314                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1315                     Break
1316                 Case MAbout%        '停止を選択した場合
1317                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1318                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1319                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1320                     Break
1321                 Case MNgProcess%    'NGを選択した場合
1322                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1323                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1324                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1325                     Break
1326                 Case MContinue%     '継続を選択した場合
1327                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1328                     M_20# = MContinue%
1329                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1330                     Break
1331             End Select
1332         EndIf
1333     EndIf
1334 '----------D720 -> D1300 コピー要求----------
1335     M_Out(12565) = 1
1336     Dly 0.5
1337     M_Out(12565) = 0
1338 '----------通信確認をする----------
1339     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1340     MRtn = 0                ' 初期化
1341     M_20# = MClear%         ' 初期化
1342     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1343     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1344     If MRtn <> 1 Then
1345         If M_20# = MContinue% Then
1346             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1347         Else
1348             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1349         EndIf
1350     EndIf
1351 '----------工程抜け確認----------
1352     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1353     MRtn = 0                ' 初期化
1354     M_20# = MClear%         ' 初期化
1355     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1356     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1357     If MRtn <> 1 Then
1358         If M_20# = MContinue% Then
1359             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1360         Else
1361             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1362         EndIf
1363     EndIf
1364     '
1365     fnPiasCheck = 1
1366     *fnPiasCheck_End
1367     Exit Function
1368 FEnd
1369 '
1370 '■fnPCComuCheck
1371 ''' <summary>
1372 ''' PC-PLC通信チェック
1373 ''' </summary>
1374 ''' <returns>   0 : NG
1375 '''             1 : OK(読込み完了)
1376 ''' </returns>
1377 ''' <remarks>
1378 ''' Date   : 2021/07/07 : M.Hayakawa
1379 ''' </remarks>'
1380 Function M% fnPCComuCheck
1381     fnPCComuCheck = 0
1382     MJudge% = 0                                  '初期化
1383     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1384     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1385     '
1386     For MStaNo = 0 To 5
1387         '
1388         If M_In(MIN_PIAS_ComOK%) = 1 Then
1389             'PC通信OK(M400)
1390             MJudge% = MOK%
1391             MStaNo = 5
1392             Break
1393         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1394             'toRBT_通信確認time out
1395             MJudge% = MNG%
1396             MCommentD1001 = 15
1397             MCommentD1002 = 21
1398             MStaNo = 5
1399             Break
1400         Else
1401             'toRBT_通信確認time out
1402             MJudge% = MNG%
1403             MCommentD1001 = 14
1404             MCommentD1002 = 21
1405             Break
1406         EndIf
1407     Next MStaNo
1408     '
1409     '上記で返信フラグを受信してからPC通信確認OFF
1410     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1411     '
1412     'エラー画面
1413     If MJudge% <> MOK% Then
1414         M_20# = MClear%     '初期化
1415         'エラー処理記述
1416         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1417         'GOT KEY入力待ち
1418         MKeyNumber = fnKEY_WAIT()
1419         '
1420         If MKeyNumber = MAbout% Then            '停止を選択した場合
1421             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1422             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1423             Break
1424         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1425             M_20# = MPass%                      'M_20# プログラム間共通外部変数
1426             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1427             Break
1428         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1429             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1430             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1431             Break
1432         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1433             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1434             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1435             Break
1436         EndIf
1437     Else
1438         'OKの場合
1439         fnPCComuCheck = 1
1440     EndIf
1441     Exit Function
1442 FEnd
1443 '
1444 '■fnProcessCheck
1445 ''' <summary>
1446 ''' 工程抜け確認
1447 ''' </summary>
1448 ''' <returns>    1：工程履歴OK     0：異常終了
1449 '''             -1：前工程履歴NG  -2：自工程履歴あり
1450 '''             -3：モデル仕向NG  -4：タイムアウト
1451 '''             -5：履歴処理エラー
1452 ''' </returns>
1453 ''' <remarks>
1454 ''' Date   : 2021/07/07 : M.Hayakawa
1455 ''' </remarks>'
1456 Function M% fnProcessCheck
1457     fnProcessCheck = 0
1458     MJudge% = MNG%      '一旦NGを初期化とする
1459 '----------工程抜け確認----------
1460     MCommentD1001 = 0   'コメント初期化
1461     For MStaNo = 0 To 5
1462         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1463         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1464         '
1465         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1466             MJudge% = MOK%
1467             fnAutoScreenComment(85)     ' AUTO画面
1468             MStaNo = 5
1469             Break
1470         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1471             MFlgLoop% = 0
1472             MJudge% = MNG%
1473             MCommentD1001 = 27
1474             MCommentD1002 = 22
1475             fnAutoScreenComment(94)     ' AUTO画面
1476             fnProcessCheck = -2         ' NGは-2を返す
1477             MStaNo = 5
1478             Break
1479         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1480            MJudge% = MNG%
1481             MCommentD1001 = 31
1482             MCommentD1002 = 22
1483             fnAutoScreenComment(83)     ' AUTO画面
1484             fnProcessCheck = -3         ' NGは-3を返す
1485             MStaNo = 5
1486             Break
1487         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1488             '履歴NGは直ぐに終了せず繰り返し確認を行う
1489             '前工程の書込みが終了していない可能性があるため
1490             MJudge% = MNG%
1491             MCommentD1001 = 32
1492             MCommentD1002 = 22
1493             fnAutoScreenComment(84)     ' AUTO画面
1494             fnProcessCheck = -1         ' NGは-1を返す
1495             Dly 1.0
1496             '工程抜け確認OFF
1497             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1498             Dly 1.0
1499            'MStaNo = 5
1500             Break
1501         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1502             MFlgLoop% = 0
1503             MJudge% = MNG%
1504             MCommentD1001 = 29
1505             MCommentD1002 = 22
1506             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1507             fnProcessCheck = -5         ' NGは-5を返す
1508             MStaNo = 5
1509             Break
1510         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1511             MJudge% = MNG%
1512             If MCommentD1001 = 32 Then
1513                 '何もしない
1514             Else
1515                 MCommentD1001 = 26
1516             EndIf
1517             MCommentD1002 = 22
1518             fnProcessCheck = -4         ' NGは-4を返す
1519             MStaNo = 5
1520             Break
1521         Else
1522             MJudge% = MNG%
1523             MCommentD1001 = 28
1524             MCommentD1002 = 22
1525         EndIf
1526     Next MStaNo
1527     '工程抜け確認OFF
1528     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1529     '通過履歴NG 工程抜けの場合
1530     If MJudge% = MPass% Then
1531         M_20# = MPass%
1532     EndIf
1533     '
1534     'エラー画面
1535     If MJudge% <> MOK% Then
1536         M_20# = MClear%     '初期化
1537         'エラー処理記述
1538         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1539         'GOT KEY入力待ち
1540         MKeyNumber = fnKEY_WAIT()
1541         '
1542         Select MKeyNumber
1543             Case MAbout%        '停止を選択した場合
1544                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1545                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1546                 Break
1547             Case MNext%         '次へを選択した場合
1548                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1549                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1550                 Break
1551             Case MContinue%     '継続を選択した場合
1552                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1553                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1554                 Break
1555             Case MNgProcess%    'NGを選択した場合
1556                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1557                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1558                 Break
1559         End Select
1560     Else
1561         fnProcessCheck = 1  ' OKは1を返す
1562     EndIf
1563     Exit Function
1564 FEnd
1565 '
1566 '■fnPiasWrite
1567 ''' <summary>
1568 ''' Pias 組立結果書込み要求
1569 ''' </summary>
1570 '''<param name="MFlg%">
1571 '''                 MOK%(1) = 工程履歴にOKを書込む
1572 '''                 MNG%(0) = 工程履歴にNGを書込む
1573 '''</param>
1574 '''<returns></returns>
1575 ''' <remarks>
1576 ''' Date   : 2021/07/07 : M.Hayakawa
1577 ''' </remarks>'
1578 Function M% fnPiasWrite(ByVal MFlg%)
1579       fnPiasWrite = 0
1580 *RETRY_PIASWRITE
1581     '
1582     '組立OK(MOK%)の場合　M306 ON
1583    '組立NG(MNG%)の場合　M307 ON
1584     If MFlg% = MOK% Then
1585         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1586     Else
1587         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1588     EndIf
1589     Dly 0.1                  '念のため
1590     '
1591     'Piasへ書込み開始 M305 -> ON
1592     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1593     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1594     '
1595     MJudge% = MNG%
1596     '
1597     For MStaNo = 0 To 5
1598         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1599             MJudge% = MOK%
1600             'MRet = fnAutoScreenComment(85)  'AUTO画面
1601             MStaNo = 5
1602             Break
1603         '
1604         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1605             MJudge% = MNG%
1606             'MRet = fnAutoScreenComment(85)  'AUTO画面
1607            MCommentD1001 = 34
1608            MCommentD1002 = 25
1609             MStaNo = 5
1610             Break
1611         '
1612         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1613             MJudge% = MNG%
1614             'MRet = fnAutoScreenComment(85)  'AUTO画面
1615            MCommentD1001 = 35
1616            MCommentD1002 = 25
1617             MStaNo = 5
1618             Break
1619         '
1620         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1621             MJudge% = MNG%
1622             'MRet = fnAutoScreenComment(85)  'AUTO画面
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
1637     'Piasへ書込み開始 M305 -> OfF
1638     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1639     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1640     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1641     '
1642     '
1643     '通過履歴NG 工程抜けの場合
1644     If MJudge% = MPass% Then
1645         M_20# = MPass%
1646     EndIf
1647     '
1648    M_20# = MClear%     '初期化
1649     '
1650     'エラー画面
1651     If MJudge% < MOK% Then
1652     '
1653 '残しておくが現状では使用しないラベル
1654 *RETRY_ERR_WRITE
1655         M_20# = MClear%     '初期化
1656         'エラー処理記述
1657         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1658         'GOT KEY入力待ち
1659         MKeyNumber = fnKEY_WAIT()
1660         '
1661         If MKeyNumber = MAbout% Then   '停止を選択した場合
1662             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1663            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1664             Break
1665         '
1666         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1667             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1668             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1669         '
1670         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1671             M_20# = MPass%            'M_20# プログラム間共通外部変数
1672             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1673         '
1674         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1675             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1676            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
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
1691 '■fnPCBNumberCheck
1692 ''' <summary>
1693 ''' Pias 基板番号照合要求
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
1705     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1706     'Piasへ基板照合開始 M310 -> ON
1707     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1708     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1709     '
1710     MJudge% = MNG%
1711     '
1712     For MStaNo = 0 To 5
1713         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1714             MJudge% = MOK%
1715             fnAutoScreenComment(96)  'AUTO画面
1716             MStaNo = 5
1717             Break
1718         '
1719         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1720             MJudge% = MNG%
1721             fnAutoScreenComment(97)  'AUTO画面
1722             MCommentD1001 = 37
1723             MCommentD1002 = 25
1724             MStaNo = 5
1725             Break
1726         '
1727         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1728             MJudge% = MNG%
1729             fnAutoScreenComment(98)  'AUTO画面
1730             MCommentD1001 = 38
1731             MCommentD1002 = 25
1732             MStaNo = 5
1733             Break
1734         '
1735         ElseIf M_In(11580) = 1 Then                         'time out
1736             MJudge% = MNG%
1737             fnAutoScreenComment(99)  'AUTO画面
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
1752     'Piasへ基板照合開始 M310 -> OfF
1753     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1754     '
1755     '
1756     '通過履歴NG 工程抜けの場合
1757     If MJudge% = MPass% Then
1758         M_20# = MPass%
1759     EndIf
1760     '
1761    M_20# = MClear%     '初期化
1762     '
1763     'エラー画面
1764     If MJudge% < MOK% Then
1765     '
1766 '残しておくが現状では使用しないラベル
1767 *RETRY_ERR_PCBNUMBER
1768         M_20# = MClear%     '初期化
1769         'エラー処理記述
1770         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1771         'GOT KEY入力待ち
1772         MKeyNumber = fnKEY_WAIT()
1773         '
1774         If MKeyNumber = MAbout% Then   '停止を選択した場合
1775             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1776             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1777             Break
1778         '
1779         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1780             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1781             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1782         '
1783         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1784             M_20# = MNext%            'M_20# プログラム間共通外部変数'MPass%→MNext%へ変更(12/7中村)
1785             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1786         '
1787         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1788             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1789             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
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
1802 '■ScrewTight_S2
1803 ''' <summary>
1804 ''' ねじ締めを行う
1805 ''' </summary>
1806 '''<param name="PScrewPos()">
1807 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1808 '''             PScrewPos(2)    ：ねじ締め回避点
1809 '''             PScrewPos(10)   ：ねじ締め終了高さ
1810 '''</param>
1811 '''<returns>整数
1812 '''         0=異常終了、1=正常終了
1813 '''</returns>
1814 ''' <remarks>
1815 ''' Date   : 2021/07/07 : M.Hayakawa
1816 ''' </remarks>'
1817 Function M% ScrewTight_S2(ByVal PScrewPosition())   'ネジ締め個別設定
1818     ScrewTight_S2 = 0
1819     MOKNGFlg = 0
1820     Ovrd 100
1821     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1822     ' 暫定
1823     Ovrd 5
1824     Mvs PScrewPosition(10),-10    ' パレット上ねじ締めS①の上空へ移動
1825 '    Ovrd MOvrdA
1826     '暫定マスク
1827 '    M_Out(Y62_Driver)=1     ' バンクセッティング　C1
1828 '    Dly 0.1
1829 '    M_Out(Y61_Driver)=1     'ドライバーON　CW
1830 '    'Spd 8.3 '外部ライド100  内部ライド60   'ライド100-40　100%：Spd　15　'ねじ締め速度設定
1831 '    Spd MSpdA               'ネジ締め時Spd個別設定
1832     ' 暫定移動のみ
1833     Mvs PScrewPosition(10)
1834 '    '
1835 '    Dly 0.1
1836 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1837 '    Wait M_In(11584)=1          '完了/エラー検出
1838 '    Dly 0.1
1839 '    Spd M_NSpd
1840 '    '
1841 '    If M_In(X28_Driver)=1 Then  'ねじトータルエラー検出時
1842 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1843 '        Dly 0.1
1844 '        M_Out(Y62_Driver)=0     'バンクセッティング解除　C1
1845 '        Dly 0.1
1846 '        M_Out(Y63_Driver)=0     'バンクセッティング解除　C1
1847 '        Dly 0.1
1848 '        M_Out(Y65_Driver)=0     'プログラム解除　F1
1849 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1850 '        M_Out(Y68_VV1)=0        'ねじ吸着　OFF
1851 '        MOKNGFlg = -1
1852 '        ScrewTight_S2 = 0
1853 '    Else
1854 '        Wait M_In(X29_Driver)=1 ' 正常完了時
1855 '        Dly 0.1
1856 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1857 '        Dly 0.1
1858 '        M_Out(Y62_Driver)=0     'バンクセッティング解除
1859 '        Dly 0.1
1860         M_Out(12249)=1 Dly 0.3         'ねじ吸着　OFF (一時コメントアウト解除,(Y68_VV1)=0を(12249)=1 Dly 0.3に変更(8/5中村))
1861         M_Out(12250)=1 Dly 0.1         '暫定真空破壊
1862 '        Dly 0.1
1863 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1864 '        ScrewTight_S2 = 1
1865 '    EndIf
1866 ' 暫定
1867     Ovrd 10
1868     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1869     Ovrd 100
1870     Exit Function
1871 FEnd
1872 '
1873 '■ScrewGet_S3
1874 ''' <summary>
1875 ''' ねじ供給機からねじを得る
1876 ''' </summary>
1877 '''<param name="%"></param>
1878 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1879 '''         PScrewPos(2)    ：ねじ供給器回避点
1880 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1881 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1882 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1883 '''<returns>整数
1884 '''         0=異常終了、1=正常終了、-1=MネジセンサーNG、-2=MネジセンサーON、-3=吸着エラー
1885 '''</returns>
1886 ''' <remarks>
1887 ''' Date   : 2021/07/07 : M.Hayakawa
1888 ''' </remarks>'
1889 Function M% ScrewGet_S3(ByVal PScrewPosition())
1890     ScrewGet_S3 = 0
1891     MMScrewJudge% = 0
1892     'ねじ供給器初期動作エラーチェック
1893 ' ↓暫定削除
1894 '    Wait M_In(X34_ScrewReady1)=1 'ねじ供給器SがReadyになるまで待つ　←　何秒か待ってReadyにならなければ抜けるプログラムが必要？
1895 '    Ovrd 100
1896 '    If M_In(X33_SS2)=0 Then  'Mねじ検出センサがOFF（故障）していた場合
1897 '        Ovrd 30
1898 '        Mvs,-80             'その場所から80mm上空へ移動
1899 '        Mov PInitPos19049   '19049初期位置へ移動
1900 '        M_Out(Y68_VV1)=0    'ねじ吸着 Off
1901 '        'NGとしてここの関数から抜ける
1902 '        ScrewGet_S3 = -1
1903 '        MMScrewJudge% = 1
1904 '        MCommentD1001 = 61
1905 '    EndIf
1906 '    If ScrewGet_S3 = 0 Then
1907 '        'Sタイト用ねじ供給機にMねじが混入していないか監視
1908 '        MMScrewJudge% = 0 'MMScrewJudgeを初期化する
1909 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
1910 '        If MRtn = 0 Then
1911 '            Ovrd 30
1912 '            Mvs,-80            'その場所から50mm上空へ移動
1913 '            Mov PInitPos19049  '19049初期位置へ移動
1914 '            MMScrewJudge% = 2
1915 '            MRtn = All_CLamp_Release()'全てのクランプ解除へ分岐
1916 '            MCnt% = 2   '2を設定
1917 '            MCommentD1001 = 62
1918 '        EndIf
1919 '        If MMScrewJudge% = 2 Then
1920 '            ScrewGet_S3 = -2
1921 '        EndIf
1922 '    EndIf
1923 '    'Mネジ判定がONの場合 NGとして関数を抜ける
1924 '    If MMScrewJudge% = 2 Then
1925 '        ScrewGet_S3 = -2
1926 '    EndIf
1927     'Sネジ用ねじ太郎のMネジ混入確認用ここまで
1928     Ovrd 100
1929     Spd M_NSpd
1930     If MMScrewJudge% = 0 Then
1931         ScrewGet_S3 = 0
1932         M_Out(Y63_Driver)=1         ' バンクセッティング　C2
1933         MScrewCnt% = 0
1934         MFinCnt% = 2
1935 '        For MCnt% = 0 To MFinCnt%
1936             Mov PScrewPosition(2)        ' ねじ供給機回避点
1937             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1938             Ovrd 80
1939             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1940             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
1941             Mvs PScrewPosition(10), 1.2
1942             M_Out(Y68_VV1)=1 Dly 0.3        ' ねじ吸着　ON
1943             'ビット回転
1944             M_Out(Y60_Driver)=1
1945             Dly 0.2
1946             '
1947             Ovrd 100
1948             JOvrd M_NJovrd
1949             Spd M_NSpd
1950             'ネジ吸着確認位置移動
1951             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
1952             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
1953             'ビット回転停止
1954             'M_Out(Y60_Driver)=0
1955             '
1956             '1秒間ネジ吸着確認
1957 ' 以下暫定削除
1958 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
1959 '            'MRtn = 0'強制エラー
1960 '            '吸着エラーの場合
1961 '            'ネジをねじ太郎に戻す
1962 '            If MRtn = 0 Then
1963 '                Ovrd 30
1964 '                'ビット回転停止
1965 '                M_Out(Y60_Driver)=0
1966 '                'ネジ供給機上空
1967 '                Mvs PScrewPos(1)
1968 '                '更に上空
1969 '                Mov PScrewPos(1), -75
1970 '                'ネジ捨て位置
1971 '                Mov PScrewFeedS021
1972 '                '吸着OFF
1973 '                M_Out(Y68_VV1)=0 'ねじ吸着　OFF
1974 '                Dly 0.2
1975 '                '破壊ON
1976 '                M_Out(Y6B_VB1)=1 '真空破壊ON
1977 '                'ビット回転
1978 '                M_Out(Y61_Driver)=1
1979 '                Dly 0.5
1980 '                '
1981 '                Ovrd 100
1982 '                JOvrd M_NJovrd
1983 '                Spd M_NSpd
1984 '                'ドライバーを上下させねじを振り落とす
1985 '                Mov PScrewFeedS021, 10
1986 '                Mov PScrewFeedS021
1987 '                Dly 0.1
1988 '                Mov PScrewFeedS021, 10
1989 '                Mov PScrewFeedS021
1990 '                '
1991 '                'ネジ落ち待ち
1992 '                'ビット回転停止
1993 '                M_Out(Y61_Driver)=0
1994 '                Dly 0.1
1995 '                '破壊OFF
1996 '                M_Out(Y6B_VB1)=0 '真空破壊OFF
1997 '                '
1998 '                '
1999 '                'ねじ落ちたとして、移動更に上空
2000 '                Mov PScrewPos(1), -75
2001 '                Ovrd 100
2002 '                Spd M_NSpd
2003 '                'ネジ供給機上空
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
2017         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2018         M_Out(Y60_Driver)=0     ' ビット回転停止
2019         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
2020         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2021         'もう一度吸着確認
2022 ' 以下暫定削除
2023 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2024 '        If MRtn = 0 Then      '吸着エラーの場合
2025 '            MCommentD1001 = 94
2026 '            MCommentD1002 = 95
2027 '            ScrewGet_S3 = -3
2028 '        EndIf
2029 '        If MRtn = 1 Then      '吸着OKの場合
2030 '            ScrewGet_S3 = 1
2031 '        EndIf
2032 '        Break
2033     Else
2034         'Mネジ
2035         If MMScrewJudge% = 2 Then
2036             ScrewGet_S3 = -2
2037         EndIf
2038     EndIf
2039     Exit Function
2040 FEnd
2041 '
2042 '■fnKEY_WAIT()
2043 ''' <summary>
2044 ''' GOTからのキー入力待ち
2045 ''' </summary>
2046 '''<returns>1：停止    2：次へ
2047 '''         3：継続    4：トルクチェック開始
2048 '''         5：NG
2049 '''         11：ロボット初期位置1    12：ロボット初期位置2
2050 '''         13：ロボット初期位置3    14：ロボット初期位置4
2051 '''</returns>
2052 ''' <remarks>
2053 ''' Date   : 2021/07/07 : M.Hayakawa
2054 ''' </remarks>'
2055 Function M% fnKEY_WAIT()
2056     fnKEY_WAIT = 0
2057     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
2058     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
2059     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
2060     '下記キー待ちの継続に反応させないため
2061     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
2062     Dly 0.2
2063     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
2064     MLocalLoopFlg=1
2065     While MLocalLoopFlg=1
2066         If M_In(11345) = 1 Then         '停止   M5345
2067             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
2068             fnKEY_WAIT = 1
2069             MLocalLoopFlg=-1
2070             Break
2071         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
2072             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
2073             fnKEY_WAIT = 2
2074             MLocalLoopFlg=-1
2075             Break
2076         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
2077             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
2078             fnKEY_WAIT = 3
2079             MLocalLoopFlg=-1
2080             Break
2081         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
2082             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
2083             fnKEY_WAIT = 4
2084             MLocalLoopFlg=-1
2085             Break
2086         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
2087             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
2088             fnKEY_WAIT = 5
2089             MLocalLoopFlg=-1
2090             Break
2091             '
2092         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
2093             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
2094             fnKEY_WAIT = MRobotInit1%
2095             MLocalLoopFlg=-1
2096             Break
2097             '
2098         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
2099             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
2100             fnKEY_WAIT = MRobotInit2%
2101             MLocalLoopFlg=-1
2102             Break
2103             '
2104         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
2105             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
2106             fnKEY_WAIT = MRobotInit3%
2107             MLocalLoopFlg=-1
2108             Break
2109             '
2110         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2111             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2112             fnKEY_WAIT = MRobotInit4%
2113             MLocalLoopFlg=-1
2114             Break
2115             '
2116         Else
2117         EndIf
2118     WEnd
2119     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2120     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2121     Exit Function
2122 FEnd
2123 '
2124 '■ fnAUTO_CTL
2125 ''' <summary>
2126 ''' AUTOモードOFF、PLCからの開始待ち
2127 ''' </summary>
2128 ''' <remarks>
2129 ''' Date   : 2021/07/07 : M.Hayakawa
2130 ''' </remarks>
2131 Function M% fnAUTO_CTL
2132     fnAUTO_CTL = 0
2133     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2134     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2135     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2136     '
2137     If M_Svo=0 Then             'サーボON確認
2138         Servo On
2139     EndIf
2140     Wait M_Svo=1
2141     Exit Function
2142 FEnd
2143 '
2144 '■ fnWindScreenOpen
2145 ''' <summary>
2146 ''' ウィンド画面の表示、非表示設定
2147 ''' </summary>
2148 '''<param name="%"></param>
2149 '''<param name="%"></param>
2150 '''<param name="%"></param>
2151 '''<param name="%"></param>
2152 ''' <remarks>
2153 ''' コメントD1001, D1002, D1003の設定
2154 ''' MWindReSet = 0     画面非表示
2155 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2156 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2157 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2158 ''' Date   : 2021/07/07 : M.Hayakawa
2159 ''' </remarks>
2160 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2161     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2162         M_Out16(12480) = MCommentD1001            'D1001 コメント
2163     EndIf
2164     '
2165     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2166         M_Out16(12496) = MCommentD1002            'D1002 コメント
2167     EndIf
2168     '
2169     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2170        M_Out16(12512) = MCommentD1003            'D1003 コメント
2171     EndIf
2172     '
2173     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2174     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2175     Dly 0.5
2176     M_Out(12363) = 0                         'ウィンド画面設定
2177     Exit Function
2178 FEnd
2179 '
2180 '■FnCtlValue2
2181 ''' <summary>
2182 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2183 ''' </summary>
2184 ''' <param name="MCtlNo%"></param>
2185 ''' <remarks>
2186 ''' Date : 2022/04/28 渡辺
2187 ''' </remarks>
2188 '''
2189 '''  1：投入数       ＋１
2190 '''  2：組立ＯＫ数   ＋１
2191 '''  3：組立ＮＧ数   ＋１ (未使用)
2192 '''  4：吸着エラー数 ＋１
2193 ''' 99：読書開始信号 OFF
2194 '''
2195 Function M% FnCtlValue2(ByVal MCtlNo%)
2196     FnCtlValue2 = 1
2197     Select MCtlNo%
2198         Case 1        '投入数＋１
2199             M_Out(12569) = 0             '書込み開始信号OFF
2200             M_Out(12568) = 1             '読込み開始信号ON
2201             MInputQty = M_In16(11600)    '投入数受信
2202             MInputQty = MInputQty + 1    '投入数＋１
2203             M_Out16(12592) = MInputQty   '投入数送信
2204             M_Out(12569) = 1             '書込み開始信号ON
2205             Break
2206             '
2207         Case 2        '組立ＯＫ数＋１
2208             M_Out(12569) = 0             '書込み開始信号OFF
2209             M_Out(12568) = 1             '読込み開始信号ON
2210             MAssyOkQty = M_In16(11616)   '組立OK数受信
2211             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2212             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2213             M_Out(12569) = 1             '書込み開始信号ON
2214             Break
2215             '
2216         Case 4        '吸着エラー数＋１
2217             M_Out(12569) = 0                       '書込み開始信号OFF
2218             M_Out(12568) = 1                       '読込み開始信号ON
2219             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2220             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2221             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2222             M_Out(12569) = 1                       '書込み開始信号ON
2223             Break
2224             '
2225         Case 99        '読書開始信号OFF
2226             M_Out(12568) = 0        '読込み開始信号OFF
2227             M_Out(12569) = 0        '書込み開始信号OFF
2228             Break
2229             '
2230     End Select
2231 FEnd
2232 'Insightによる画像処理検査実行（並列処理なし）
2233 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2234 '-------------------------------------------------------------------------------
2235 'Insightによる画像処理検査実行（並列処理なし）
2236 '   引数
2237 '       PInspPos()      ：検査位置
2238 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2239 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2240 '       MInspCnt%       ：検査位置数
2241 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2242 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2243 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2244 '   戻り値：整数
2245 '       0=異常終了、1=正常終了
2246 '
2247 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2248 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2249 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2250 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2251 '   20200410    :   検査グループ設定Retry追加
2252 '-------------------------------------------------------------------------------
2253     '----- 初期設定 -----
2254     Cnt 0                                                           '移動効率化解除(初期値=0)
2255     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2256 '    Cnt 1,0.1,0.1
2257     '変数宣言・初期化
2258     Def Inte MNum                                                   '検査番号(検査順1～)
2259     MNum% = 1                                                       '検査番号初期値設定
2260     Def Inte MEndFlg                                                '検査終了フラグ
2261     MEndFlg% = 0
2262     '
2263     '検査G番号設定要求・検査実行要求off
2264     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2265     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2266     'エラー番号クリア
2267     MInspErrNum = 0                                                 '検査実行エラー番号
2268     M_Out16(MOUT_InspErrNum) = MInspErrNum
2269     MInspNGStepNum = 0                                              '検査実行NGStep番号
2270     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2271     '
2272     'Insight Ready check?
2273     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2274         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2275         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2276         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2277         ISInspectionSingle = 0                                      '異常終了戻り値設定
2278         Exit Function
2279     EndIf
2280     '
2281     '検査位置数確認
2282     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2283         MInspErrNum = 21                                            '検査データなし 21　引数<1
2284         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2285         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2286         ISInspectionSingle = 0                                      '異常終了戻り値設定
2287         Exit Function
2288     EndIf
2289     '
2290     '
2291     '
2292     '----- メイン処理 -----
2293     '設定された検査位置数分の検査実行
2294     While( MEndFlg% = 0 )
2295         '----- 検査グループ番号設定Retry追加 20200410
2296         MSetGrNumRetryExitFlg = 0
2297         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2298         While( MSetGrNumRetryExitFlg = 0 )
2299         '----- 検査グループ番号設定Retry追加ここまで 20200410
2300             '
2301             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2302             '
2303             '----- 検査グループ番号設定 -----
2304             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2305             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2306             '
2307             '検査位置へ移動・移動完了待ち
2308             Mvs PInspPos( MNum% )                                       '移動
2309             Dly 0.05                                                    '移動完了後Delay
2310             '
2311             '検査グループ番号設定終了確認
2312             M_Timer(1) = 0
2313             MExitFlg = 0
2314             While( MExitFlg = 0 )
2315                 '検査G設定正常終了?
2316                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2317                     MExitFlg = 1
2318                 '
2319                 '検査G設定異常終了?
2320                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2321                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2322                     If MInspErrNum = 0 Then                             '1回目のエラー?
2323                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2324                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2325                     EndIf
2326                     MExitFlg = 1
2327                 '
2328                 'timeoutチェック
2329                 ElseIf 1000 < M_Timer(1) Then
2330                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2331                     If MInspErrNum = 0 Then                             '1回目のエラー?
2332                         MInspErrNum = 12                                'timeout エラー番号=12
2333                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2334                     EndIf
2335                     MExitFlg = 1
2336                 EndIf
2337             WEnd
2338             '
2339             '検査G番号設定要求off
2340             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2341             '
2342             '----- 検査グループ設定Retry追加 20200410
2343             'NGなければ抜ける
2344             If MCurrentStepErr = 0 Then
2345                 MSetGrNumRetryExitFlg = 1
2346             Else
2347                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2348                 If MSetGrNumRetryCnt = 0 Then
2349                     MSetGrNumRetryExitFlg = 1
2350                 Else
2351                     'Retryへ　その前にDelay
2352                     Dly 0.5
2353                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2354                 EndIf
2355             EndIf
2356             '----- 検査グループ設定Retry追加ここまで 20200410
2357             '
2358         WEnd
2359         '
2360         '
2361         '
2362         '----- 検査実行 -----
2363         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2364             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2365                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2366                 MInspRetryExitFlg = 0
2367                 MRetryCnt = 2                                        'Retry回数設定
2368                 While( MInspRetryExitFlg = 0 )
2369                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2370                     '
2371                     '検査完了確認
2372                     MRetryCnt = MRetryCnt - 1
2373                     M_Timer(1) = 0
2374                     MExitFlg = 0
2375                     While( MExitFlg = 0 )
2376                     '検査完了待ち
2377                         '検査OK終了?
2378                         If M_In( MIN_IS_InspOK% ) = 1  Then
2379                             MJudgeOKFlg = 1                         '検査OKフラグON
2380                             MExitFlg = 1
2381                         '
2382                         '検査NG終了?
2383                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2384                             If MInspErrNum = 0 Then                 '1回目のエラー?
2385                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2386                                     MInspErrNum = 32                    '検査NG エラー番号=32
2387                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2388                                 EndIf
2389                             EndIf
2390                             MExitFlg = 1
2391                         '
2392                         '検査異常終了(IS timeout)?
2393                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2394                             If MInspErrNum = 0 Then                 '1回目のエラー?
2395                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2396                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2397                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2398                                 EndIf
2399                             EndIf
2400                             MExitFlg = 1
2401                         '
2402                         'timeoutチェック
2403                         ElseIf 3000 < M_Timer(1) Then
2404                             If MInspErrNum = 0 Then                 '1回目のエラー?
2405                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2406                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2407                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2408                                 EndIf
2409                             EndIf
2410                             MExitFlg = 1
2411                         EndIf
2412                     WEnd
2413                     '
2414                     '検査開始要求off
2415                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2416                     '
2417                     'OKなら抜ける
2418                     If MJudgeOKFlg = 1 Then
2419                         MInspRetryExitFlg = 1
2420                     Else
2421                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2422                         If MRetryCnt = 0 Then
2423                             MInspRetryExitFlg = 1
2424                         Else
2425                             'Retryへ　その前にDelay
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
2436         MNum% = MNum% + 1                                           '検査Step+1
2437         '検査終了確認　検査終了フラグセット
2438         If (MInspCnt% < MNum% ) Then
2439             MEndFlg% = 1                                            '検査終了フラグセット
2440         EndIf
2441         'NG発生時続行時処理
2442         If MInspErrNum <> 0 Then                                    'NGあり?
2443             If MNgContinue% <> 1 Then                               'NG続行?
2444                 MEndFlg% = 1                                        '検査終了フラグセット
2445             EndIf
2446         EndIf
2447     WEnd
2448     '
2449     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2450     If 0 < MZAxis% Then
2451         PCurrentPos = P_Curr                                        '現在位置取得
2452         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2453         Mvs PCurrentPos                                             '現在位置上空へ移動
2454     EndIf
2455     '
2456     '戻り値設定
2457     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
2458         ISInspectionSingle = 1                                      '正常終了戻り値設定
2459     Else
2460         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2461         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2462         ISInspectionSingle = 0                                      '異常終了戻り値設定
2463     EndIf
2464     '
2465     Fine 0 , P
2466     Exit Function
2467 FEnd
2468 '
2469 '■InitialZone
2470 ''' <summary>
2471 ''' 初期動作で現在位置を読み込んで最寄りの待避場所に移動
2472 ''' 設定された待避位置付近の場合はなにもしない。
2473 ''' それ以外の場合はオーバーライドを下げて移動。
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
2563             'ハンドをイニシャルに戻す
2564             M_Out(12256) = 0    '本体チャック閉OFF
2565             M_Out(12257) = 1    '本体チャック開ON
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
2577             'ハンドをイニシャルに戻す
2578             M_Out(12256) = 0    '本体チャック閉OFF
2579             M_Out(12257) = 1    '本体チャック開ON
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
2621 '■MoveToShuntPosition
2622 ''' <summary>
2623 ''' 現在位置を読み込んで最寄りの待避場所に移動
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
2721 '■fnAutoScreenComment
2722 ''' <summary>
2723 ''' メイン画面の動作状況表示
2724 ''' コメントD1005の設定
2725 ''' </summary>
2726 '''<param name="McommentD1005%">コメントID</param>
2727 ''' <remarks>
2728 ''' Date   : 2021/07/07 : M.Hayakawa
2729 ''' </remarks>
2730 Function fnAutoScreenComment(ByVal McommentD1005%)
2731     M_Out16(12576) = McommentD1005%
2732     Exit Function
2733 FEnd
2734 '
2735 '■InitialZoneB
2736 ''' <summary>
2737 ''' 非常停止後の復帰動作
2738 ''' 1)上空退避　Z方向上に移動
2739 ''' 2)J1軸以外を退避ポジションへ移動
2740 ''' 3)J1軸のみを退避ポジションへ移動
2741 ''' 4)イニシャルポジションへ移動
2742 ''' </summary>
2743 ''' <remarks>
2744 ''' Date : 2022/03/22 : N.Watanabe
2745 ''' </remarks>
2746 Function V fnInitialZoneB()
2747     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/04/27 渡辺
2748 '
2749 'パラメータ
2750     Ovrd 5
2751 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2752 '    Cmp Pos, &B100011
2753 '
2754 '復帰動作開始
2755 '
2756 '置き台と両掴みの場所は、チャックを解放する    '2022/04/15 渡辺
2757 *RecoveryChuckOpen
2758     PActive = P_Curr          '現在位置を取得
2759     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
2760 'PProductOnRoboSet(ねじロボ1本体置き位置)は、チャック解放
2761     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2762         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2763             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2764                 MRecoveryChuckOpen = 1
2765             EndIf
2766         EndIf
2767     EndIf
2768 'PProductOnRoboGet(ねじロボ1本体取り位置)は、チャック解放
2769     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2770         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2771             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2772                 MRecoveryChuckOpen = 1
2773             EndIf
2774         EndIf
2775     EndIf
2776 '
2777     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2778     M_Out(12256) = 0                           '本体チャック閉OFF
2779     M_Out(12257) = 1                           '本体チャック開ON
2780 '
2781     M_20# = 0                                  'KEY入力初期化
2782     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
2783     If MRtn = 1 Then M_Out(12257) = 0          '本体チャック開OFF
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
2794 '上空退避
2795     PActive = P_Curr
2796     Pmove = PActive
2797     Pmove.Z = 640           '上空退避する一律の高さ
2798     If PActive.X > 550 Then
2799         Pmove.Z =480        'パレット上に腕を伸ばしているときは640まで上げられない為、例外処置
2800     EndIf
2801     If PActive.Z < Pmove.Z Then   '現在の高さがPmoveより低いとのみ実行
2802         Mvs Pmove
2803     EndIf
2804 '
2805     Dly 1.0
2806 'J1軸以外を退避ポジションへ移動
2807     JActive = J_Curr
2808     Jmove = JTaihi
2809     Jmove.J1 = JActive.J1        'J1軸のみ現在値を使用し、他の軸はJTaihiのポーズを取る
2810     Mov Jmove
2811     Dly 1.0
2812 'J1軸のみを退避ポジションへ移動
2813     Mov JTaihi
2814     Dly 1.0
2815 'イニシャルポジションへ移動
2816     Mov PInitialPosition
2817     Cmp Off
2818 ' ねじロボを初期位置に戻すために強制的に自動運転開始
2819     If M_In(11856) = 0 Then                 ' 停止中のみ
2820         fnAutoScreenComment(501)            ' 状態表示[ネジ締め機自動運転開始中] 2022/04/27 渡辺
2821         M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
2822         MRet = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
2823         If MRet = 0 Then
2824         Else
2825             M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
2826         EndIf
2827     EndIf
2828     M_Out(12264) = 0            '位置決め出OFF
2829     M_Out(12265) = 1            '位置決め戻ON
2830    fErrorProcess(11,253,281,0)
2831     Exit Function
2832 *RecoveryEnd
2833 FEnd
2834 'f
2835 '
2836 '■fnRoboPosChk
2837 ''' <summary>
2838 ''' 最後に終了したロボットポジションの確認
2839 ''' </summary>
2840 '''<param name="MINNumber%">入力番号</param>
2841 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2842 '''<param name="MTimeCnt&">タイムアウト時間</param>
2843 ''' PLCに保続した番号を読込み、確認
2844 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2845 '''<returns>整数 0:タイムアウト 1:OK</returns>
2846 ''' <remarks>
2847 ''' Date   : 2021/07/07 : M.Hayakawa
2848 ''' </remarks>
2849 Function M% fnRoboPosChk
2850     fnRoboPosChk = 0
2851     MRet = fnStepRead()
2852     '初期位置でないと判断した場合
2853     'ウィンド画面切換え
2854     If MRBTOpeGroupNo > 5 Then
2855         '下記キー待ちの継続に反応させないため
2856         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2857         Dly 0.2
2858         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2859         Dly 1.5
2860         '
2861         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2862         '
2863         MLoopFlg% = 1
2864         While MLoopFlg% = 1
2865             '
2866             '
2867             MKeyNumber% = fnKEY_WAIT()
2868             Select MKeyNumber%
2869                 Case Is = MAbout%       '停止
2870                     M_20# = MAbout%
2871                     MLoopFlg% = -1
2872                     Break
2873                 Case Is = MNext%        '次へ
2874                     'MLoopFlg% = -1
2875                     Break
2876                 Case Is = MContinue%    '継続
2877                     M_20# = MContinue%
2878                     MLoopFlg% = -1
2879                     Break
2880                 Default
2881                     Break
2882             End Select
2883         WEnd
2884     EndIf
2885     '
2886     If M_20# = MContinue% Then                              '継続ボタンが押された場合
2887         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
2888         Ovrd 5                                   '低速オーバーライド値設定
2889         Select MRBTOpeGroupNo
2890             Case Is = 5                          '何もしない
2891                 Break
2892             Case Is = 10                         '初期位置へ戻す
2893                 'Mov PTEST001
2894                 Break
2895             Case Is = 15                         '初期位置へ戻す
2896                 'Mov PTEST002
2897                 Dly 0.5
2898                 'Mov PTEST001
2899                 Dly 0.5
2900                 Break
2901             Default
2902                 Break
2903         End Select
2904         '
2905         Ovrd M_NOvrd                            'システムの初期値を設定
2906         M_Out(12364) = 1                        'toPLC_データ保存ON
2907         MRBTOpeGroupNo = 5
2908         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
2909         Dly 1.0
2910         M_Out(12364) = 0                        'toPLC_データ保存OFF
2911         fnRoboPosChk = 1                        '初期位置動作実行
2912         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
2913     EndIf
2914     Exit Function
2915 FEnd
2916 '
2917 '■frInCheck
2918 ''' <summary>
2919 ''' センサーINチェック
2920 ''' </summary>
2921 '''<param name="MINNumber%">入力番号</param>
2922 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2923 '''<param name="MTimeCnt&">タイムアウト時間</param>
2924 '''<returns>整数 0:タイムアウト 1:OK</returns>
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
2945 'ねじ締め機通信確認
2946 '
2947 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2948 'fScewTcomChk = 0　：正常終了
2949 '          　　 -1 ：異常終了
2950 '-----------------------------------------------
2951 Function M% fScewTcomChk
2952 *ReCheckScewTcomChk
2953     fScewTcomChk = 0
2954     '通信確認送信
2955     M_Out(MOUT_ScwT_ComChk%) = MOn%
2956     '通信確認受信待機
2957 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
2958     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
2959     '通信確認送信終了
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
2971 'ねじ締め開始送信
2972 '
2973 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2974 'fScewTStart = 0　：正常終了
2975 '          　　-1 ：異常終了
2976 '-----------------------------------------------
2977 Function M% fScewTStart
2978     fScewTStart = 0
2979     nRet% = 0
2980     'ねじ締め開始待機を受信
2981 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
2982     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
2983     If MRtn = 0 Then nRet% = -1
2984     If MRtn = 0 Then GoTo *ScrewStartERROR      '開始できなかった場合ジャンプ
2985     Dly 0.1
2986     'ねじ締め開始受信を送信
2987     M_Out(MOUT_ScwT_ST%) = MOn%
2988     Dly 0.5
2989     'Wait M_In(MTEST_KEY%) = MOn%
2990     'ねじ締め開始送信終了
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
3001 'ねじ締め完了受信
3002 '
3003 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3004 'fScewTcomChk = 0　：正常終了
3005 '          　 　-1 ：異常終了
3006 '-----------------------------------------------
3007 Function M% fScewTFinish
3008 *ReCheckScewTFinish
3009     fScewTFinish = 0
3010     'ねじ締め完了待機を受信
3011 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3012     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3013     If MRtn = 0 Then
3014         fScewTFinish = -1
3015     EndIf
3016     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3017     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3018     Dly 0.1
3019     'ねじ締め完了受信を送信
3020     M_Out(MOUT_ScwT_FinOK%) = MOn%
3021     Dly 0.5                          'とりあえず保持時間0.5msec
3022     'ねじ締め開始送信終了
3023     M_Out(MOUT_ScwT_FinOK%) = MOff%
3024     'Wait M_In(MTEST_KEY%) = MOn%
3025     '
3026 *ScewTFinish_ErrEnd
3027 FEnd
3028 '
3029 '
3030 '-----------------------------------------------
3031 '
3032 '条件xx停止受信
3033 '
3034 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3035 'fScewTCaseStop = 0　：正常終了
3036 '          　   　-1 ：異常終了
3037 '-----------------------------------------------
3038 Function M% fScewTCaseStop(ByVal MCase%())
3039 *ReCheckScewTCaseStop
3040     fScewTCaseStop = 0
3041     '条件xx停止を受信
3042     Wait M_In(MCase%(1)) = MOn%
3043     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3044     If MRtn = 0 Then
3045         fScewTCaseStop = -1
3046     EndIf
3047     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3048     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3049     Dly 0.1
3050     '条件xx停止受信を送信
3051     M_Out(MCase%(2)) = MOn%
3052     Dly 0.5                          'とりあえず保持時間0.5msec
3053     'ねじ締め開始送信終了
3054     M_Out(MCase%(2)) = MOff%
3055 *ScewTCaseStop_ErrEnd
3056     '
3057 FEnd
3058 '
3059 '-----------------------------------------------
3060 '
3061 '再開始受信
3062 '
3063 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3064 'fScewTReStart = 0　：正常終了
3065 '              　-1 ：異常終了
3066 '-----------------------------------------------
3067 Function M% fScewTReStart()
3068 *ReCheckScewTReStart
3069     fScewTReStart = 0
3070     '再開始を受信
3071     Wait M_In(MIN_ScwT_ReST%) = MOn%
3072     MRtn = fTimeOutJudge(MIN_ScwT_ReST%,MOn%)
3073     If MRtn = 2 Then GoTo *ReCheckScewTReStart
3074     If MRtn = 0 Then GoTo *ScewTReStart_ErrEnd
3075     Dly 0.1
3076     '再開始受信を送信
3077     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msecパルス
3078 *ScewTReStart_ErrEnd
3079     Exit Function
3080 FEnd
3081 '
3082 '■fScrewTighenRoboCheck
3083 '<summary>
3084 'ねじロボ監視
3085 '</summary>
3086 '<param name = "MStopNum%"> 停止番号</param>
3087 '<returns>整数 0:ねじロボ異常終了 1:OK </returns>
3088 '<make>
3089 '2021/12/2 中村天哉
3090 '</make>
3091 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3092     fnAutoScreenComment(503)    '状態表示[ねじロボ動作終了待ち] 2022/04/27 渡辺
3093     fScrewTighenRoboCheck = 1
3094     MScrewTighenRoboFlg% = 1    'フラグの初期化
3095     MCheck% = 0
3096     While MScrewTighenRoboFlg% = 1
3097         MCheck% = M_In16(11904)
3098         If M_In(MStopNum%) = 1 Then '停止位置まで来たら
3099             MScrewTighenRoboFlg% = 0 '関数を抜ける
3100             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/27 渡辺
3101         EndIf
3102         If MCheck% <> 0 Then
3103             fScrewTighenRoboError(MCheck%)
3104             Select M_20#
3105                 Case MAbout%            '停止が押された場合
3106                     M_Out(12869) = 1 Dly 1.0
3107                     MScrewTighenRoboFlg% = 0
3108                     fScrewTighenRoboCheck = 0   '異常終了
3109                     Break
3110                 Case MNgProcess%        'NGが押された場合
3111                     M_Out(12873) = 1 Dly 1.0
3112                     MScrewTighenRoboFlg% = 0
3113                     fScrewTighenRoboCheck = 0   '異常終了
3114                     Break
3115                 Case MContinue%             'リトライが押された場合
3116                     M_20# = MClear%         'M_20#初期化
3117                     M_Out(12871) = 1 Dly 1.0
3118                     Break
3119                 Case MNext%                 '次へが押された場合
3120                     M_20# = MClear%         'M_20#初期化
3121                     M_Out(12874) = 1 Dly 1.0
3122                     Break
3123             End Select
3124             Dly 0.5
3125         EndIf
3126     WEnd
3127     Exit Function
3128 FEnd
3129 '■fScrewTighenRoboError
3130 '<summary>
3131 'ねじロボエラー処理
3132 '</summary>
3133 '<param name = "ErrorCode%"> エラー番号</param>
3134 '<make>
3135 '2021/12/2 中村天哉
3136 '</make>
3137 Function fScrewTighenRoboError(ErrorCode%)
3138     MCommentD1001 = ErrorCode% + 300
3139     fErrorProcess(11,MCommentD1001,0,0)
3140     Exit Function
3141 FEnd
3142 '■fErrorProcess
3143 '<summary>
3144 'エラー処理
3145 '</summary>
3146 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
3147 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
3148 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
3149 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
3150 '<make>
3151 '2021/11/5 中村天哉
3152 '</make>
3153 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3154     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
3155     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
3156     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
3157     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
3158 *RETRY_ERR_PROCESS
3159      M_20# = MClear%     '初期化
3160 '        'エラー処理記述
3161         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3162 '        'GOT KEY入力待ち
3163         MKeyNumber = fnKEY_WAIT()
3164 '        '
3165         If MKeyNumber = MAbout% Then   '停止を選択した場合
3166             M_20# = MAbout%            'M_20# プログラム間共通外部変数
3167  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3168             Break
3169          '
3170         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
3171             M_20# = MContinue%            'M_20# プログラム間共通外部変数
3172  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3173             Break
3174         '
3175         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
3176             M_20# = MNext%            'M_20# プログラム間共通外部変数
3177  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3178             Break
3179          '
3180         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
3181             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
3182  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3183             Break
3184         '
3185         EndIf
3186         '
3187         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3188         fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3189     Exit Function
3190 FEnd
3191 '
3192 '■fnTorqueCheck
3193 ''' <summary>
3194 ''' トルクチェック動作用のメイン
3195 ''' </summary>
3196 ''' <remarks>
3197 ''' Date   : 2021/12/21 : H.AJI
3198 ''' </remarks>'
3199 Function M% fnTorqueCheck
3200     'トルクチェック中送信  搬送系停止
3201     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
3202     '
3203     fnTorqueCheck = 0
3204     Ovrd 20
3205     'Mov PInitialPosition              '初期位置移動
3206     Ovrd 100
3207     '下記キー待ちの継続に反応させないため
3208     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3209     Dly 0.2
3210     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3211     '
3212     'M6340  トルクチェック受信
3213     'Dly 5.0
3214     M_Out(12340) = 1          'トルクチェック受信 M6340
3215     Dly 1.0
3216     M_Out(12340) = 0
3217     '
3218     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3219     M_Out(12835) = 1                         'ねじロボトルクチェック画面切替
3220    Wait M_In(11843) = 1                         'ねじロボトルクチェック画面切替
3221     M_Out(12835) = 0                         'ねじロボトルクチェック画面切替完了
3222     '
3223     '
3224     MLoopFlg = 1
3225     While MLoopFlg = 1
3226         '
3227         'Mov PInitialPosition              '初期位置移動
3228         '
3229         MKeyNumber = fnKEY_WAIT()
3230         Select MKeyNumber
3231             Case Is = 1           '停止
3232                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3233                 Dly 1.0
3234                 M_Out(12343) = 0
3235                 Ovrd 20
3236                 'Mov PTicketRead_1
3237                 M_Out(12840) = 1          'トルクチェック終了
3238                 Wait M_In(11859) = 1      'ねじロボからの終了
3239                 M_Out(12840) = 0          'トルクチェック終了
3240                 Ovrd 100
3241                 M_20# = 1
3242                 MLoopFlg = -1
3243                 Break
3244             Case Is = 2           '次へ
3245                 Break
3246             Case Is = 3           '継続
3247                 Break
3248             Case Is = 4           'トルクチェック開始
3249                 M_Out(12342) = 1          'トルクチェック開始要求受信 M6342
3250                 Dly 1.0
3251                 M_Out(12342) = 0
3252                 If M_In(11862) = 1 Then             'トルクチェッカー確認
3253                     fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3254                     MRet = fnScrewMTorque()           'ねじロボ用トルクチェック
3255                 EndIf
3256                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3257                 'MRet = fnMoveTorquePosi()
3258                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3259                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3260                 Break
3261             Default
3262                 Break
3263         End Select
3264     WEnd
3265     '
3266     'トルクチェック中停止送信
3267     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3268     '
3269     'ロボットの位置を元に戻す
3270     '
3271     Exit Function
3272  FEnd
3273  '
3274 '
3275 '
3276 '---------------------------
3277 '
3278 '    メイン画面の表示、非表示設定
3279 '         コメントD1001, D1002, D1003の設定
3280 '           MWindReSet = 0     画面非表示
3281 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3282 '           MWindErrScr = 10    エラー画面 D1001, D1002
3283 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3284 '
3285 '---------------------------
3286 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3287     fnMainScreenOpen = 0
3288     '
3289    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3290         M_Out16(12480) = MCommentD1001            'D1001 コメント
3291     EndIf
3292     '
3293     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3294         M_Out16(12496) = MCommentD1002            'D1002 コメント
3295     EndIf
3296     '
3297     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3298         M_Out16(12512) = MCommentD1003            'D1003 コメント
3299     EndIf
3300     '
3301     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3302     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3303     Dly 0.5
3304     M_Out(12362) = 0                         'ウィンド画面設定
3305     Exit Function
3306 FEnd
3307 '
3308 '■Main
3309 ''' <summary>
3310 ''' トルクチェック実動作
3311 ''' </summary>
3312 ''' <remarks>
3313 ''' Date   : 2021/12/21 : H.AJI
3314 ''' </remarks>'
3315 Function M% fnScrewMTorque
3316     fnScrewMTorque = 0
3317     M_Out(12838) = 1                         'トルクチェック開始1
3318     Wait M_In(11857) = 1                     '受信完了
3319     M_Out(12838) = 0                         'トルクチェック開始1
3320     Dly 2.0
3321     Exit Function
3322 FEnd
3323 '
3324 '
3325 '----------------------------------------------------------------
3326 'fTimeOutJudge
3327 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3328 '引数
3329 'Address% = 監視アドレス番号
3330 'JudgeFlg% = 対象アドレスの正常終了時の値
3331 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3332 '戻り値 = 0 エラー
3333 '         1 正常終了
3334 '         2 リトライ
3335 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3336 '作成日
3337 '2022/9/20 中村
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
3358 '■Main
3359 ''' <summary>
3360 ''' 組立動作用のメイン
3361 ''' </summary>
3362 ''' <remarks>
3363 ''' Date   : 2021/07/07 : M.Hayakawa
3364 ''' </remarks>'
3365 Function Main
3366     MopeNo = M_21#         '外部変数にて動作番号代入
3367     '
3368     If M_Svo=0 Then
3369         Servo On
3370     EndIf
3371     Wait M_Svo=1
3372 '組立スタート日付時刻要求パルスON
3373     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3374 'パトライト操作
3375     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3376     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3377     '
3378     M_20# = 0                                   'KEY入力初期化
3379     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
3380     MRet% = 0
3381 '初期位置の確認と移動
3382 '
3383 '復帰動作　実行・未実行判別      2022/03/22 渡辺 作成
3384     PActive = P_Curr                    '現在位置を取得
3385     MRecoveryPass% = 0
3386     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3387         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3388             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3389                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3390             EndIf
3391         EndIf
3392     EndIf
3393     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3394         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3395             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3396                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3397             EndIf
3398         EndIf
3399     EndIf
3400     If MRecoveryPass% = 0 Then
3401        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3402     EndIf
3403 '
3404 '
3405 '
3406     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3407         M_Out(12364) = 1            'toPLC_データ保存ON
3408 'トルクチェック
3409         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3410             MRet% = fnTorqueCheck()
3411             Break
3412         Else
3413 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
3414 '                MRtn = InspInit()               '画像処理初期化処理
3415 '            EndIf
3416             '
3417            M_20# = MClear%                    '初期化
3418 '組立開始
3419             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3420                 MRet% = fnAssyStart()
3421             Else
3422                 M_20# = MPass%
3423             EndIf
3424 '組立終了日付時刻
3425             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3426             Wait M_In(11572) = 1            '日付取得完了
3427             Dly 0.1
3428             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3429 'リフターユニットへのOUT
3430             '  KEY入力が何もない場合 OKと判断
3431             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3432             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3433 'OK/NGフラグ出力
3434             If M_20# <= 0 Then
3435                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3436             ElseIf M_20# = MPass% Then
3437                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3438             EndIf
3439 'PIASに組立完了書込み
3440             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3441                 If M_20# = MPass% Then
3442                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3443                 Else
3444                     'KEY入力がNGの場合
3445                     If M_20# = MNgProcess% Then
3446                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3447                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3448                         MRet% = fnPiasWrite(MNG%)
3449                        nAssyNgQty = nAssyNgQty + 1
3450                     EndIf
3451                     '
3452                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3453                     If M_20# = MAssyOK% Then
3454                             '-----------------------
3455                             'D732 -> D2600 コピー要求
3456                             M_Out(12566) = 1
3457 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3458                             M_Out(12566) = 0
3459                             '
3460                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3461                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3462                             '基板番号照合(PPは未使用）
3463 '                            MRet% = fnPCBNumberCheck()
3464                         Else
3465                             MRet% = 1
3466                         EndIf
3467                         '
3468                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3469                             If M_20# <> MAbout% Then
3470                                 '工程履歴OK書き込み
3471                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3472                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3473                                 MRet% = fnPiasWrite(MOK%)
3474                                 nAssyOkQty = 0
3475                                 nAssyOkQty = nAssyOkQty + 1
3476                             Else
3477                                 nAssyOkQty = nAssyOkQty + 1
3478                             EndIf
3479                         EndIf
3480                     EndIf
3481 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3482 '                    MRet% = fnPiasWrite(MOK%)
3483                 EndIf
3484             Else
3485                 nAssyOkQty = nAssyOkQty + 1
3486             EndIf
3487             '
3488             '組立終了日付時刻解除
3489             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3490             '投入数、組立OK数、組立NG数書込み
3491 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3492             '
3493 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3494 '                '画像処理終了処理
3495 '                MRtn = InspQuit()
3496 '            EndIf
3497         EndIf
3498         M_Out(12364) = 0                          'toPLC_データ保存OFF
3499     EndIf
3500 'パトライト操作
3501     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3502     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3503 'GOT表示
3504     fnAutoScreenComment(93)  'AUTO画面 工程完了
3505 FEnd
3506 End
3507 '
3508 'おまじないコメント
3509 '絶対削除するな
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
