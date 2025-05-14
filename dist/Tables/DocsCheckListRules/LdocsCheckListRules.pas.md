<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código `LdocsCheckListRules` implementa uma interface para gerenciar regras de checklist de documentos. Ele permite que os usuários filtrem, visualizem, modifiquem e criem novas regras relacionadas a documentos, com base em critérios como cliente, mercado, região, entre outros. O objetivo principal é facilitar a gestão e a busca de regras de checklist de documentos em um sistema corporativo.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para desenvolver a aplicação.
  - **Componentes Visuais:** Inclui componentes como `cxGrid`, `TsPanel`, `TsLabel`, e `TsCheckBox` para criar a interface gráfica.
  - **Serviços:** Utiliza serviços externos para buscar dados relacionados a clientes, mercados, regiões, etc.

* **Forma do Componente:**
  - **Formulário com Elementos de Filtro e Grid:**
    - **Elementos do Formulário:**
      - `FRAMEFindCust` (Busca por Cliente).
      - `FRAMEFindCustMkt` (Busca por Mercado do Cliente).
      - `FRAMEFindCons` (Busca por Consignatário).
      - `FRAMEFindConsMkt` (Busca por Mercado do Consignatário).
      - `FRAMEFindRegion` (Busca por Região).
      - `FRAMEFindIncoterm` (Busca por Termos de Entrega).
      - `FRAMEFindPayment` (Busca por Pagamento).
      - `FRAMEFindDocs` (Busca por Tipos de Documentos).
      - `CHKactive` (Checkbox para ativar/desativar filtros).
    - **Ações do Formulário:**
      - Botões de busca e limpeza de critérios.
      - Grid para exibição de resultados.

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Filtrar regras de checklist de documentos com base em critérios específicos.
  - Visualizar, modificar e criar novas regras.
  - Limpar critérios de busca.

* **Componentes Principais:**
  - **Grid:** Exibe os resultados das regras filtradas.
  - **Painel de Filtros:** Permite ao usuário definir critérios de busca.
  - **Botões de Ação:** Incluem botões para buscar e limpar critérios.

* **Pseudo-código de Ações e Eventos:**
  - `if botão de busca clicado then executar busca com critérios definidos`.
  - `if botão de limpar critérios clicado then limpar todos os campos de filtro`.
  - `if valor de campo alterado then validar campo`.

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do formulário e carregamento dos componentes visuais.
  2. O usuário interage com os campos de filtro e define critérios.
  3. Ao clicar no botão de busca, os critérios são validados e os resultados são exibidos no grid.
  4. O usuário pode visualizar, modificar ou criar novas regras.

* **Dados Necessários:**
  - Informações como cliente, mercado, região, tipo de documento, entre outros, devem ser preenchidas para realizar buscas específicas.

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - O botão "Buscar" só deve ser habilitado se pelo menos um critério de busca for preenchido.
  - O botão "Limpar Critérios" limpa todos os campos de filtro.

* **Filtros Disponíveis:**
  - Cliente.
  - Mercado do Cliente.
  - Consignatário.
  - Mercado do Consignatário.
  - Região.
  - Termos de Entrega.
  - Pagamento.
  - Tipo de Documento.

* **Mensagens de Erro:**
  - "Nenhum critério definido" se o botão de busca for clicado sem critérios preenchidos.
  - "Erro ao buscar dados" se houver falha na comunicação com os serviços.

* **Valores Padrão dos Campos:**
  - Não definidos explicitamente no código.

* **Validações e Condições dos Campos:**
  - Não especificadas no código.

## 5. Funções Principais:

* **Funções e Lógica de Negócio:**
  - `m_SetFindCustomer`: Configura o filtro de cliente.
  - `m_SetFindCustMarket`: Configura o filtro de mercado do cliente.
  - `m_SetFindConsignee`: Configura o filtro de consignatário.
  - `m_SetFindConsMarket`: Configura o filtro de mercado do consignatário.
  - `m_SetFindRegion`: Configura o filtro de região.
  - `m_SetFindIncoterm`: Configura o filtro de termos de entrega.
  - `m_SetFindPayment`: Configura o filtro de pagamento.
  - `m_SetFindDocTp`: Configura o filtro de tipo de documento.

## 6. Consumo de Serviços de API:

* **Chamadas a Serviços Externos:**
  - **Serviço:** `CheckListDocRulesServiceUtils`.
    - **Endpoint:** Não especificado.
    - **Dados Enviados:** Critérios de busca.
    - **Dados Recebidos:** Lista de regras de checklist.
    - **Propósito:** Buscar regras de checklist com base nos critérios.
    - **Tratamento de Erros:** Exibe mensagem de erro em caso de falha.

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos no código.

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`, `TsPanel`, `TsLabel`, `TsCheckBox`, entre outros, para componentes visuais.
  - `kneCBListSOA` e `kneFRFindEditSOA` para funcionalidades específicas.

* **Componentes Customizados:**
  - `FRAMEFindEditSOA`: Usado para filtros de busca.
  - `FRAMEBusUnit`: Usado para seleção de unidade de negócios.

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**
  - Cliente (obrigatório, tipo: string).
  - Mercado do Cliente (opcional, tipo: string).
  - Consignatário (opcional, tipo: string).
  - Mercado do Consignatário (opcional, tipo: string).
  - Região (opcional, tipo: string).
  - Termos de Entrega (opcional, tipo: string).
  - Pagamento (opcional, tipo: string).
  - Tipo de Documento (opcional, tipo: string).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não especificado no código.

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  procedure TFORMLdocsCheckListRules.m_SetFindCustomer;
  begin
    // Configura o filtro de cliente
  end;
  ```
* **HTML Representando o Formulário:**
  ```html
  <div style="width: 978px;">
    <label>Customer:</label>
    <input type="text" placeholder="Enter customer" />
    <label>Cust. Market:</label>
    <input type="text" placeholder="Enter customer market" />
    <button>Search</button>
    <button>Clear</button>
  </div>
  ```

## 11. Comentários Importantes no Código:

* O código utiliza constantes como `mc_GRID_FIELDS` para definir os campos exibidos no grid.
* Métodos como `EventSetup` e `GridSetup` são sobrescritos para configurar eventos e o grid.

## 12. Conclusão:

O código implementa uma interface robusta para gerenciar regras de checklist de documentos, com suporte a filtros avançados e integração com serviços externos. No entanto, faltam detalhes sobre validações de campos e endpoints de serviços.

## 13. Resumo Curto:

O código `LdocsCheckListRules` fornece uma interface para gerenciar regras de checklist de documentos, com filtros avançados e integração com serviços externos. Ele é parte de um sistema maior para facilitar a gestão de documentos corporativos.#### **LdocsCheckListRules.pas**

```
unit LdocsCheckListRules;

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
  sCheckBox, kneCBList, kneFRFindEditSOA, kneFRBusUnit
  ;

type
  TFORMLdocsCheckListRules = class(TFORMkneCBListSOA)
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    CHKactive: TsCheckBox;
    FRAMEFindCust: TFRAMEFindEditSOA;
    FRAMEFindCustMkt: TFRAMEFindEditSOA;
    FRAMEFindCons: TFRAMEFindEditSOA;
    FRAMEFindConsMkt: TFRAMEFindEditSOA;
    FRAMEFindRegion: TFRAMEFindEditSOA;
    FRAMEFindIncoterm: TFRAMEFindEditSOA;
    LBL1: TsLabel;
    LBL2: TsLabel;
    LBL3: TsLabel;
    LBL4: TsLabel;
    LBL5: TsLabel;
    LBL6: TsLabel;
    LBL7: TsLabel;
    LBL8: TsLabel;
    FRAMEFindPayment: TFRAMEFindEditSOA;
    FRAMEFindDocs: TFRAMEFindEditSOA;
    FRAMEBusUnit1: TFRAMEBusUnit;
    LBL9: TsLabel;
    sLabel1: TsLabel;
    FRAMEFindMill: TFRAMEFindEditSOA;
  private
    procedure m_SetFindCustomer;
    procedure m_SetFindCustMarket;
    procedure m_SetFindConsignee;
    procedure m_SetFindConsMarket;
    procedure m_SetFindRegion;
    procedure m_SetFindIncoterm;
    procedure m_SetFindPayment;
    procedure m_SetFindDocTp;
    procedure m_BeforeSearch(Sender: TObject);
    procedure m_SetFindMill;
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
  FORMLdocsCheckListRules: TFORMLdocsCheckListRules;

implementation


uses
  Global, kneFGFindUtils
  //--- Forms/Frames
  , EdocsCheckListRules
  //--- ServiceUtils
  , CheckListDocRulesServiceUtils
  , CustomerMarketServiceUtils
  , ConsigneeMarketServiceUtils
  , RegionServiceUtils
  , DeliveryTermServiceUtils
  , PaymentCustomerServiceUtils
  , CheckListDocsToAddServiceUtils
  , MillServiceUtils {#24067}
  ;

const
  mc_GRID_FIELDS = 'stat; ruleId; ruleDesc; businessUnit; lastUpd; updBy';

{$R *.dfm}

{ TFORMLdocsCheckListRules }
```

#### **LdocsCheckListRules.dfm**

```
inherited FORMLdocsCheckListRules: TFORMLdocsCheckListRules
  Left = 473
  Top = 168
  Caption = 'Documents Check List Rules'
  ClientWidth = 978
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  PixelsPerInch = 96
  TextHeight = 13
  inherited SPLlist: TsSplitter
    Top = 267
    Width = 978
  end
  inherited PNLsearchArea: TsPanel
    Width = 978
    Height = 223
    inherited PNLsearchButtons: TsPanel
      Left = 884
      Height = 221
      inherited BTsearch: TsBitBtn
        ParentFont = True
      end
      inherited BTclearCriteria: TsBitBtn
        ParentFont = True
      end
    end
    inherited SRBcriteria: TsScrollBox
      Width = 883
      Height = 221
      inherited PNLcriteria: TsPanel
        Width = 879
        Height = 217
        object LBL1: TsLabel
          Left = 8
          Top = 13
          Width = 61
          Height = 13
          Caption = 'Customer:'
          FocusControl = FRAMEFindCust
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object LBL2: TsLabel
          Left = 8
          Top = 39
          Width = 78
          Height = 13
          Caption = 'Cust. Market:'
          FocusControl = FRAMEFindCustMkt
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object LBL3: TsLabel
          Left = 8
          Top = 65
          Width = 65
          Height = 13
          Caption = 'Consignee:'
          FocusControl = FRAMEFindCons
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object LBL4: TsLabel
          Left = 8
          Top = 91
          Width = 81
          Height = 13
          Caption = 'Cons. Market:'
          FocusControl = FRAMEFindConsMkt
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object LBL5: TsLabel
          Left = 8
          Top = 117
          Width = 78
          Height = 13
          Caption = 'Cust. Region:'
          FocusControl = FRAMEFindRegion
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
```
<!-- tabs:end -->

