<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  Este código define funções globais (`fp_...`) e variáveis globais (`gv_...`) que são utilizadas em uma aplicação Delphi. Ele fornece utilitários para manipulação de datasets, validação de dados, configuração de grids, e outras operações comuns em aplicações baseadas em banco de dados. O objetivo principal é centralizar funcionalidades reutilizáveis para facilitar o desenvolvimento e a manutenção do sistema.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação principal.
  - **Componentes de Banco de Dados:** `TClientDataSet`, `TDataSet`, `TField`.
  - **Componentes Visuais:** `TcxGrid`, `TcxDateEdit`, `TsCheckBox`, `TcxDBImageComboBox`.
  - **Manipulação de Arquivos INI:** `IniFiles`.
  - **Serviços Externos:** `checkVatService`.

* **Forma do Componente:**
  Este código não é um formulário ou grid diretamente, mas sim uma unidade de código que fornece funções e variáveis globais para serem utilizadas em outros módulos da aplicação.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Manipulação de datasets (edição, leitura de valores, soma de campos).
  - Validação de datas e valores.
  - Configuração de grids (largura de colunas, estado de edição).
  - Validação de e-mails.
  - Adição de critérios de busca.

* **Componentes Principais:**
  - **Funções de Manipulação de Datasets:** `SetForEdition`, `CDSEdition`, `GetFieldValuesFromCDS`.
  - **Validações:** `m_ValidateDates`, `ValidateValues`, `ValidEmail`.
  - **Configuração de Grids:** `SetNoEdittingInGridFields`, `SetColsWidthInGrid`.
  - **Critérios de Busca:** `addCriteria`, `FreeServiceCriteria`.

* **Tradução para Pseudo-código:**
  - Evento de validação de datas: `if data inicial > data final then retornar falso`.
  - Função de soma de campos: `somar valores de um campo específico no dataset`.
  - Configuração de estado de edição: `se campo for especificado, desabilitar edição no grid`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização da aplicação carrega as variáveis globais e funções.
  2. Interações do usuário (ex.: clique em botões, edição de campos) chamam funções globais para manipular datasets ou validar dados.
  3. Funções específicas são executadas conforme necessário, como validação de e-mails ou configuração de grids.

* **Dados Necessários:**
  - Datasets para manipulação.
  - Campos e valores para validação.
  - Configurações de grids (ex.: largura de colunas).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **SetForEdition:** Requer um dataset válido para habilitar o modo de edição.
  - **m_ValidateDates:** Requer datas válidas para comparação.
  - **ValidEmail:** Requer uma string no formato de e-mail.

* **Filtros Disponíveis:**
  - Critérios de busca podem ser adicionados com `addCriteria`.

* **Mensagens de Erro:**
  - "Datas inválidas" se a data inicial for maior que a final.
  - "E-mail inválido" se o formato do e-mail não for válido.

* **Valores Padrão dos Campos:**
  - `gv_DefaultBusUnit`: Valor padrão da unidade de negócio.

* **Validações e Condições dos Campos:**
  - Validação de e-mails com regex.
  - Validação de datas para garantir consistência.

---

## 5. Funções Principais:

* **SetForEdition:** Habilita o modo de edição em um dataset.
* **CDSEdition:** Verifica se um dataset está em modo de edição.
* **GetFieldValuesFromCDS:** Retorna valores de campos de um dataset como string.
* **m_ValidateDates:** Valida se a data inicial é menor ou igual à data final.
* **ValidEmail:** Valida o formato de um e-mail.
* **SetColsWidthInGrid:** Configura a largura das colunas de um grid.

---

## 6. Consumo de Serviços de API:

* **Serviço Externo:**
  - **Nome do Serviço:** `checkVatService`.
  - **Propósito:** Não especificado no código fornecido.

---

## 7. Campos Condicionais (Lógica de Formulário):

* Não aplicável, pois o código não contém lógica de exibição condicional de campos.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `SysUtils`, `Classes`, `DB`, `DBTables`, `DBGrids`, `Graphics`, `IniFiles`, `Controls`, `WinTypes`, `ExtCtrls`, `Dialogs`, `Variants`.
  - Componentes de terceiros como `cxCalendar`, `sCheckBox`, `kneConfigObjects`, `cxDBEdit`, `cxGridDBTableView`.

* **Componentes Customizados:**
  - `kneCBListSOA`, `kneFRGridEditSOA`.

---

## 9. Listagem de Campos e Validações:

* **Campos e Validações:**
  - `gv_Str` (tipo: string, uso auxiliar).
  - `gv_Msg` (tipo: string, mensagens).
  - `gv_DefaultBusUnit` (tipo: string, valor padrão da unidade de negócio).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não especificado no código.

---

## 10. Exemplos e Diagramas:

* **Diagramas:** Não aplicável.
* **Exemplo de Uso:**
  ```delphi
  var
    CDS: TClientDataSet;
  begin
    SetForEdition(CDS);
    if CDSEdition(CDS) then
      ShowMessage('Dataset em modo de edição');
  end;
  ```

---

## 11. Comentários Importantes no Código:

* **gc_POSTCODEKEY:** Constante para formatação de códigos postais.
* **gc_SEPARATOR:** Separador padrão para strings.

---

## 12. Conclusão:

O código fornece uma base sólida de funções e variáveis globais para manipulação de datasets, validação de dados e configuração de grids. Sua principal limitação é a falta de documentação detalhada sobre algumas funções e dependências externas.

---

## 13. Resumo Curto:

Este código define funções e variáveis globais para manipulação de datasets, validação de dados e configuração de grids em Delphi, centralizando funcionalidades reutilizáveis para facilitar o desenvolvimento e manutenção de aplicações baseadas em banco de dados.#### **Global.pas**

```
unit Global;
{
 PROPRIET�RIO   : SOPORCEL 1998
 M�DULO         : $Workfile: $
 DESCRI��O      : Fun��es (fp_...) / Variaveis Globais da Aplica��o (gv_..)
 IT LOGIN       : (Login)
 LIB EXTERNAS   :
 NOTAS          :

 �LTIMA REVIS�O :
 --------------------------------------------------------------------------
 Nr. : $Revision: 1.26 $
 DATA / HORA : $Date: 2025/02/06 10:00:50 $
 AUTOR: $Author: ibsantos $

 OUTRAS REVIS�ES:

 Nr. ,  Data,  Projecto do VSS, Coment�rios
 --------------------------------------------------------------------------
 $History: $
 $NoKeywords: $
}

interface

uses SysUtils, Classes, DB, DBTables, DBGrids, Graphics, IniFiles, Controls,
     WinTypes, ExtCtrls, Dialogs, Variants,cxCalendar, sCheckBox, DBClient,
     kneConfigObjects, cxDBEdit, cxGridDBTableView, kneCBListSOA, sDBEdit,
     kneFRGridEditSOA, cxGrid, cxEditRepositoryItems, cxImageComboBox
     , StdCtrls  ,checkVatService  , XSBuiltIns; 

{ Declara��o Tipos Globais  }
{ type}

{ Declara��o Constantes  Globais  }
const
  gc_POSTCODEKEY = 'POSTFORMAT_';

  gc_SEPARATOR: string = ';';

 {Declara��o Variaveis  Globais}
var
 gv_Str             : String;      {String Auxiliar}
 gv_Msg             : String;      {String para mensagens}
 gv_DefaultBusUnit  : String;      {Valor default do Business Unit}
 gv_BusUnitList     : String;

  procedure SetForEdition(lv_CDS: TClientDataSet);    overload;
  procedure SetForEdition(pv_DS: TDataSet);           overload;
  function CDSEdition(lv_CDS: TClientDataSet): Boolean;
  function GetFieldValuesFromCDS(const pv_dataset: TDataset;
           const pv_fieldName: string; pv_Separador: String = '|'): string;
  function fg_CalcFieldSum(
    const pv_dataset: TDataset; const pv_fieldName: string; pv_SelectFieldName: string): Currency;
  function GetFieldsValuesFromCDS(const pv_dataset: TDataset;
    const pv_fieldsName: string; pv_Values: TStringList; pv_Separador: String = '|'): Boolean;

  function fg_SetFieldWithValue(const pv_dataset: TClientDataSet;
    const pv_fieldName, pv_Value: string; pv_Filter: string = '';
    pv_SelectFieldName: string = ''; pv_SelectValue: string = ''): Boolean;
  //---
  function GetDateTimeFromServer: TDateTime;

  procedure m_SetCheckDateClick(pv_Check: TsCheckBox;
    pv_Date1, pv_Date2: TcxDateEdit);
  function m_ValidateDates(pv_IniDt, pv_FimDt: TcxDateEdit): Boolean;    overload;
  function m_ValidateDates(pv_IniDt, pv_FimDt: TcxDBDateEdit): Boolean;  overload;

  Function ValidateValues(pv_Min, pv_Max: TcxDBMaskEdit; lv_cds: TClientDataSet): Boolean;

  procedure SetNoEdittingInGridFields(pv_Fields : string; pv_form: TFORMkneCBListSOA);  overload;
  procedure SetNoEdittingInGridFields(pv_Fields : string; pv_form: TFRAMEBaseGridEditSOA);  overload;

  procedure SetColsWidthInGrid(pv_colsWidth: String; pv_gridView: TcxGridDBTableView);
  Function m_ValidateValues(pv_dataset:Tdataset; pv_limInf, pv_limSup: TsDBEdit): Boolean;

  function SetFieldReadOnlyState(const pv_Field: TField; pv_State:Boolean): Boolean;

  function SetFocusinFieldGrid(pv_FieldName: String; pv_GRD: TcxGrid;
    pv_GView: TcxGridDBTableView): Boolean;

  function ValidEmail(email: string): boolean;

//  procedure SetProtectFieldsState(Sender: TFRAMEBaseGridEditSOA);
//
//  function  SetAllControlsState(const pv_Control: TControl; const pv_Enabled: Boolean): Boolean;
//
//  procedure DoProtectControl(Sender: TObject; const pv_Args: array of const);
//

// ------------- Criteria ------------------------------------------------------
function addCriteria(var
  pv_criteria: TArrayOfFieldCriteria; pv_logical, pv_field, pv_operator,
  pv_value: string): Boolean;

function FreeServiceCriteria(pv_criteria: TArrayOfFieldCriteria):
  TArrayOfFieldCriteria;

// ------------- Utils ---------------------------------------------------------
procedure AddTocxImgCombobox(pv_Combo: TcxDBImageComboBox;
```

#### **Global.dfm**

```
dfm file is not needed for this file```
<!-- tabs:end -->

