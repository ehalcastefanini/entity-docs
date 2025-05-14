<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa uma interface para a exibição e manipulação de listas de clientes. Ele permite que os usuários filtrem, pesquisem e visualizem informações relacionadas a clientes, mercados e unidades de negócios. A funcionalidade principal é fornecer uma interface de usuário para gerenciar listas de clientes com critérios de busca avançados.

* **Tecnologias Utilizadas:**
  - Delphi (VCL Framework).
  - Componentes de interface gráfica como `TsLabel`, `TsEdit`, `TsPanel`, `TsBitBtn`, e `cxGrid`.
  - Manipulação de dados com `DBClient` e `DataSource`.

* **Forma do Componente:**
  - **Grid Display:**
    - **Colunas do Grid e seus Tipos:**
      - `stat`: Status (provavelmente string).
      - `custListCd`: Código da Lista de Clientes (string).
      - `reference`: Referência (string).
      - `marketCd`: Código do Mercado (string).
      - `lastUpd`: Última Atualização (data/hora).
      - `updBy`: Atualizado Por (string).
    - **Ações do Grid e seus Efeitos:**
      - Ações como "Novo", "Modificar", "Visualizar" e "Pesquisar" permitem manipular os dados exibidos no grid.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Criar uma nova lista de clientes.
  - Modificar uma lista existente.
  - Visualizar detalhes de uma lista.
  - Pesquisar listas de clientes com critérios avançados.

* **Componentes Principais:**
  - `EDTcode`: Campo de entrada para código de referência.
  - `FRAMEFindCust`: Componente para busca de clientes.
  - `FRAMEFindMarket`: Componente para busca de mercados.
  - `FRAMEBusUnit1`: Componente para seleção de unidade de negócios.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão de pesquisa: `se botão de pesquisa clicado então executar função de busca`.
  - Evento `OnChange` do campo de entrada: `se valor do campo alterado então validar entrada`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização do formulário carrega os componentes da interface.
  - Usuário interage com os campos de entrada e botões.
  - Eventos como cliques em botões ou alterações em campos disparam funções específicas.

* **Dados Necessários:**
  - Código de referência.
  - Cliente.
  - Mercado.
  - Unidade de negócios.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Botão "Pesquisar" só deve ser habilitado se pelo menos um critério de busca for preenchido.

* **Filtros Disponíveis:**
  - Código de referência.
  - Cliente.
  - Mercado.
  - Unidade de negócios.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo necessário estiver vazio.
  - "Critério de busca inválido" se os critérios não forem válidos.

* **Valores Padrão dos Campos:**
  - Unidade de negócios: padrão para a unidade de negócios padrão global.

* **Validações e Condições dos Campos:**
  - `EDTcode`: Deve ser validado para aceitar apenas caracteres alfanuméricos e ser convertido para maiúsculas.

---

## 5. Funções Principais:

* **Descrição das Funções:**
  - `m_SetFindCustomer`: Configura o componente de busca de clientes.
  - `m_SetFindMarket`: Configura o componente de busca de mercados.
  - `SetupParams`: Configura os parâmetros iniciais do formulário.
  - `InitCriteria`: Inicializa os critérios de busca.

---

## 6. Consumo de Serviços de API:

* **Chamadas a Serviços Externos:**
  - Serviço: `CustomerListServiceUtils`.
    - Endpoint: Não especificado no código.
    - Dados enviados: Critérios de busca.
    - Dados recebidos: Lista de clientes.
    - Propósito: Buscar listas de clientes com base nos critérios fornecidos.

---

## 7. Campos Condicionais (Lógica do Formulário):

* O campo "Unidade de Negócios" é configurado com base na lista de unidades de negócios globais e na unidade padrão.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `kneCBListSOA`: Gerenciamento de listas.
  - `kneFRFindEditSOA`: Componente de busca avançada.
  - `kneFRBusUnit`: Gerenciamento de unidades de negócios.

* **Componentes Customizados:**
  - `TFORMkneCBListSOA`: Classe base para formulários de listas.

---

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**
  - `EDTcode` (tipo: string, obrigatório, maiúsculas).
  - `FRAMEFindCust` (tipo: componente de busca, obrigatório).
  - `FRAMEFindMarket` (tipo: componente de busca, opcional).
  - `FRAMEBusUnit1` (tipo: componente de seleção, obrigatório).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `stat`: Status.
  - `custListCd`: Código da Lista de Clientes.
  - `reference`: Referência.
  - `marketCd`: Código do Mercado.
  - `lastUpd`: Última Atualização.
  - `updBy`: Atualizado Por.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  var
    Form: TFORMLcustLists;
  begin
    Form := TFORMLcustLists.Create(nil);
    try
      Form.ShowModal;
    finally
      Form.Free;
    end;
  end;
  ```
* **HTML Renderizado:**
  ```html
  <div style="font-family: Verdana; color: #5059883;">
    <label>R&eference:</label>
    <input type="text" style="text-transform: uppercase; width: 280px;" />
    <label>C&ustomer:</label>
    <input type="text" style="width: 280px;" />
    <label>Mar&ket:</label>
    <input type="text" style="width: 280px;" />
    <label>&Business Unit:</label>
    <select>
      <option>Default Business Unit</option>
    </select>
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* `mc_GRID_FIELDS`: Define os campos exibidos no grid.
* `m_SetFindCustomer` e `m_SetFindMarket`: Configuram os critérios de busca.

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar listas de clientes com critérios de busca avançados. Ele é extensível e utiliza componentes customizados para facilitar a reutilização. No entanto, a documentação de endpoints de serviços externos poderia ser mais detalhada.

---

## 13. Resumo Curto:

O código implementa um formulário para gerenciar listas de clientes, permitindo busca avançada e manipulação de dados. Ele utiliza componentes customizados e integra serviços externos para buscar informações de clientes e mercados.#### **LcustLists.pas**

```
unit LcustLists;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBListSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, knePrivileges, DBClient,
  cxEditRepositoryItems, ImgList, ActnList, sSkinProvider, ExtCtrls,
  sBevel, StdCtrls, sLabel, sScrollBox, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, kneEnterAsTab, kneFRGridManager, Buttons,
  sSpeedButton, ToolWin, ComCtrls, acCoolBar, sBitBtn, sPanel, sSplitter,
  sRadioButton, sEdit, sCheckBox, kneCBList, kneUtils, cxContainer,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxImageComboBox, DMskin, sDBText,
  kneFRFindEditSOA, sComboBox, kneFRBusUnit;

type
  TFORMLcustLists = class(TFORMkneCBListSOA)
    LBL2: TsLabel;
    EDTcode: TsEdit;
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    LBL3: TsLabel;
    FRAMEFindCust: TFRAMEFindEditSOA;
    LBL4: TsLabel;
    FRAMEFindMarket: TFRAMEFindEditSOA;
    FRAMEBusUnit1: TFRAMEBusUnit;
    LBL1: TsLabel;
  private
    procedure m_AfterSearch(Sender: TObject);
    procedure m_SetFindCustomer;
    procedure m_SetFindMarket;
    procedure m_BeforeSearch(Sender: TObject);
    procedure m_BeforeFindCustomer(Sender: TObject);
    procedure m_FreeListServiceCriteria(sender: TObject);
    { Private declarations }
  protected
    { Protected declarations }
    procedure EventSetup;           override;
    procedure GridSetup;            override;
    function SetupParams: Boolean;  override;
    procedure InitCriteria;         override;
    Procedure SetCriteriaValues;

  public
    { Public declarations }
    class function CreateListForm(
      const AOwner: TComponent): TFORMkneCBList;                   virtual;
    class procedure Initialize(const pv_FormList: TFORMkneCBList); override;
    procedure CreateEditor;                                        override;
  end;

var
  FORMLcustLists: TFORMLcustLists;

implementation


uses
  Global, kneFGFindUtils
  //--- Forms/Frames
  , EcustLists
  //--- ServiceUtils
  , CustomerListServiceUtils
  , CustomerMarketServiceUtils
  , CustomerWithGenericCriteriaServiceUtils
  ;

const
  mc_GRID_FIELDS = 'stat; custListCd; reference; marketCd'
    + '; lastUpd; updBy';

{$R *.dfm}

{ TFORMLcustLists }

class function TFORMLcustLists.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLcustLists.Create(AOwner);

  Initialize(Result);


  with TFORMLcustLists(Result) do
  begin
    m_SetFindCustomer;
    m_SetFindMarket;

    FRAMEBusUnit1.DataSource     := nil;
    FRAMEBusUnit1.BusUnitList    := gv_BusUnitList;
    FRAMEBusUnit1.BusUnitDefault := gv_DefaultBusUnit;
    FRAMEBusUnit1.IncludeAll     := True;

```

#### **LcustLists.dfm**

```
inherited FORMLcustLists: TFORMLcustLists
  Left = 473
  Top = 168
  Caption = 'Customer Lists List'
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  PixelsPerInch = 96
  TextHeight = 13
  inherited SPLlist: TsSplitter
    Top = 142
  end
  inherited PNLsearchArea: TsPanel
    Height = 98
    inherited PNLsearchButtons: TsPanel
      Height = 96
      inherited BTsearch: TsBitBtn
        ParentFont = True
      end
      inherited BTclearCriteria: TsBitBtn
        ParentFont = True
      end
    end
    inherited SRBcriteria: TsScrollBox
      Height = 96
      inherited PNLcriteria: TsPanel
        Height = 92
        object LBL2: TsLabel
          Left = 8
          Top = 13
          Width = 63
          Height = 13
          Caption = 'R&eference:'
          FocusControl = EDTcode
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object LBL3: TsLabel
          Left = 8
          Top = 39
          Width = 61
          Height = 13
          Caption = 'C&ustomer:'
          FocusControl = FRAMEFindCust.FE
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object LBL4: TsLabel
          Left = 8
          Top = 65
          Width = 44
          Height = 13
          Caption = 'Mar&ket:'
          FocusControl = FRAMEFindMarket.FE
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object LBL1: TsLabel
          Left = 410
          Top = 13
          Width = 81
          Height = 13
          Caption = '&Business Unit:'
          FocusControl = FRAMEBusUnit1.CBObusUnit
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object EDTcode: TsEdit
          Left = 81
          Top = 8
          Width = 280
          Height = 21
          CharCase = ecUpperCase
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          SkinData.SkinSection = 'EDIT'
          BoundLabel.Indent = 0
          BoundLabel.Font.Charset = DEFAULT_CHARSET
          BoundLabel.Font.Color = clWindowText
```
<!-- tabs:end -->

