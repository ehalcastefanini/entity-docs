<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa uma interface para a gestão de uma lista de clientes. Ele permite visualizar, pesquisar, modificar e gerenciar informações relacionadas aos clientes, como dados básicos, informações de portal, e configurações específicas. O objetivo principal é fornecer uma interface gráfica para facilitar a interação do usuário com os dados de clientes.

* **Tecnologias Utilizadas:**
  - Delphi (VCL - Visual Component Library).
  - Componentes de terceiros como `TsLabel`, `TsPanel`, `TcxImageComboBox`, entre outros.
  - Banco de dados para manipulação de dados via `TDataSet` e `TClientDataSet`.

* **Forma do Componente:**
  - **Grid Display:**
    - **Colunas do Grid:**
      - Código do Cliente (DBTXTcode).
      - Nome Abreviado (DBTXTabbreviatedName).
      - Nome Completo (DBTXTname).
      - Código do País (DBTXTcountryCode).
      - Descrição do País (DBTXTcountryDesc).
      - Código do Idioma (DBTXTlanguageCode).
      - Última Atualização (DBLlastUpd).
      - Atualizado Por (DBLupdBy).
    - **Ações do Grid:**
      - Seleção de registros para visualização ou edição.
      - Alteração do registro focado no grid.

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Visualizar lista de clientes.
  - Pesquisar clientes com critérios avançados.
  - Modificar informações de clientes.
  - Gerenciar informações de portal e configurações específicas.

* **Componentes Principais:**
  - **Grid de Clientes:** Exibe a lista de clientes com informações básicas.
  - **Painel de Pesquisa:** Permite definir critérios de pesquisa.
  - **Painel de Ações:** Contém botões para executar ações como salvar, cancelar, e modificar.

* **Pseudo-código de Ações e Eventos:**
  - `OnShow` do formulário: `ao exibir o formulário, inicializar os dados e carregar a lista de clientes`.
  - `OnClick` do botão "Endereço e Documento": `se botão clicado, abrir tela de endereço e documentos`.
  - `OnClick` do botão "OK": `se botão clicado, salvar alterações`.
  - `OnClick` do botão "Cancelar": `se botão clicado, descartar alterações`.
  - `OnChange` do registro no grid: `se registro focado mudar, atualizar informações exibidas`.

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. O formulário é inicializado e os componentes da interface são carregados.
  2. A lista de clientes é exibida no grid.
  3. O usuário pode interagir com os botões e campos para realizar ações como pesquisa, modificação ou visualização de detalhes.
  4. Eventos como cliques em botões ou mudanças no grid disparam funções específicas.

* **Dados Necessários:**
  - Critérios de pesquisa (opcionais).
  - Informações do cliente para modificação (nome, país, idioma, etc.).

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Botão "Salvar" (BTNok): Habilitado apenas se os campos obrigatórios forem preenchidos.
  - Botão "Cancelar" (BTNcancel): Sempre habilitado.

* **Filtros Disponíveis:**
  - Canal de Negócios (CBObusChannel).
  - Status (CBOstat).
  - Outros critérios definidos no painel de pesquisa.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se algum campo obrigatório estiver vazio.
  - "Formato de e-mail inválido" se o e-mail não for válido.

* **Valores Padrão dos Campos:**
  - Nenhum valor padrão explicitamente definido no código.

* **Validações e Condições dos Campos:**
  - Campo "E-mail de E-commerce" (EDTecommerceEmail): Deve ser validado para formato de e-mail.
  - Campo "Agregador de Cliente" (EDTclientAggr): Deve aceitar apenas texto.

## 5. Funções Principais:

* **FormShow:** Inicializa o formulário e carrega os dados.
* **ACTaddressAndDocExecute:** Abre a tela de endereço e documentos.
* **BTNokClick:** Salva as alterações realizadas.
* **BTNcancelClick:** Cancela as alterações realizadas.
* **PGCportalInfoChange:** Gerencia a mudança de abas no painel de informações do portal.

## 6. Consumo de Serviços de API:

* Não há chamadas explícitas a serviços externos no código fornecido.

## 7. Campos Condicionais (Lógica do Formulário):

* O campo "Informações do Portal" (PNLportalInfo) é exibido apenas se o usuário clicar no botão correspondente.

## 8. Dependências:

* **Bibliotecas Externas:**
  - `TsLabel`, `TsPanel`, `TcxImageComboBox`: Componentes visuais para construção da interface.
  - `TClientDataSet`: Manipulação de dados em memória.

* **Componentes Customizados:**
  - `FRAMEfindCriteriaListCustomer`: Gerencia os critérios de pesquisa.

## 9. Listagem de Campos e Validações:

* **Campos:**
  - Código do Cliente (DBTXTcode): Tipo string, obrigatório.
  - Nome Abreviado (DBTXTabbreviatedName): Tipo string, opcional.
  - Nome Completo (DBTXTname): Tipo string, obrigatório.
  - Código do País (DBTXTcountryCode): Tipo string, obrigatório.
  - Descrição do País (DBTXTcountryDesc): Tipo string, opcional.
  - E-mail de E-commerce (EDTecommerceEmail): Tipo string, obrigatório, formato de e-mail.

* **Mapeamento de Valores:**
  - Os valores exibidos no grid são mapeados diretamente para os campos do banco de dados.

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  procedure TFORMLcustomer.BTNokClick(Sender: TObject);
  begin
    if ValidarCampos then
      SalvarAlteracoes;
  end;
  ```
* **HTML Representando o Grid:**
  ```html
  <table style="width:100%; border:1px solid black;">
    <tr>
      <th>Código</th>
      <th>Nome Abreviado</th>
      <th>Nome Completo</th>
      <th>Código do País</th>
      <th>Descrição do País</th>
    </tr>
    <tr>
      <td>001</td>
      <td>ABC</td>
      <td>Cliente ABC</td>
      <td>BR</td>
      <td>Brasil</td>
    </tr>
  </table>
  ```

## 11. Comentários Importantes no Código:

* O evento `FormShow` é essencial para inicializar os dados e carregar a lista de clientes.
* O botão "Endereço e Documento" (`ACTaddressAndDocExecute`) é usado para abrir uma tela específica.

## 12. Conclusão:

O código fornece uma interface robusta para a gestão de clientes, com funcionalidades de pesquisa, visualização e modificação. No entanto, a validação de campos e mensagens de erro poderiam ser mais detalhadas. Além disso, a integração com APIs externas não está presente.

## 13. Resumo Curto:

O código implementa uma interface para gerenciar clientes, permitindo pesquisa, visualização e edição de dados. Ele utiliza componentes visuais e manipulação de dados em memória, mas carece de integração com APIs externas e validações mais robustas.#### **Lcustomer.pas**

```
unit Lcustomer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBListSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, DBClient,
  cxEditRepositoryItems, ImgList, ActnList, sSkinProvider, ExtCtrls,
  sBevel, StdCtrls, sLabel, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, Buttons, sSpeedButton, kneFRGridManager,
  ToolWin, ComCtrls, acCoolBar, sBitBtn, sPanel, sSplitter, kneCBList,
  sDBText, sScrollBox, kneFRfindCriteria, kneEnterAsTab, FRfindCriteriaCustomer,
  FRfindCriteriaListCustomer, knePrivileges, cxGridDBDataDefinitions,
  sButton, sEdit, sCheckBox, sMemo, sPageControl, kneFREditSOA,
  kneFRGridEditSOA, FRcustCrosssellBrand, sDBCheckBox, cxContainer,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxImageComboBox, sComboBox;

type
  TFORMLcustomer = class(TFORMkneCBListSOA)
    LBL1: TsLabel;
    BVL2: TsBevel;
    DBTXTcode: TsDBText;
    sLabel2: TsLabel;
    sBevel3: TsBevel;
    DBLstat: TsDBText;
    DBLlastUpd: TsDBText;
    DBLupdBy: TsDBText;
    DBTXTabbreviatedName: TsDBText;
    DBTXTname: TsDBText;
    sLabel1: TsLabel;
    sBevel2: TsBevel;
    DBTXTlanguageCode: TsDBText;
    sLabel3: TsLabel;
    sBevel4: TsBevel;
    DBTXTcountryCode: TsDBText;
    DBTXTcountryDesc: TsDBText;
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    ACTaddressAndDoc: TAction;
    PNL1: TsPanel;
    BTNaddress: TsBitBtn;
    cxEDTvalidVAT: TcxEditRepositoryImageComboBoxItem;
    IMLvat: TImageList;
    FRAMEfindCriteriaListCustomer1: TFRAMEfindCriteriaListCustomer;
    PNLchgPortalInfo: TsPanel;
    BTNchgPortalInfo: TsBitBtn;
    ACTchgPortalInfo: TAction;
    PNLportalInfo: TsPanel;
    SRBportalInfo: TsScrollBox;
    LBLremarks: TsLabel;
    LBLecommerceEmail: TsLabel;
    EDTecommerceEmail: TsEdit;
    EDTremarks: TsEdit;
    PNLbtns: TsPanel;
    BTNcancel: TsButton;
    BTNok: TsButton;
    CBXportalFlag: TsCheckBox;
    CBXallowRappel: TsCheckBox;
    CBXallowEnquires: TsCheckBox;
    CBXallowRfq: TsCheckBox;
    CBXallowClaims: TsCheckBox;
    CBXallowMkt: TsCheckBox;
    LBLclientAggr: TsLabel;
    EDTclientAggr: TsEdit;
    FRAMEcustCrosssellBrand1: TFRAMEcustCrosssellBrand;
    PGCportalInfo: TsPageControl;
    TSHportalInfoPrincipal: TsTabSheet;
    TSHcrossSellingBrands: TsTabSheet;
    CHKallowCrosssell: TsCheckBox;
    LBLchannelSource: TsLabel;
    CBOchannelSource: TcxImageComboBox;
    CBXallowBudget: TsCheckBox;
    CBXallowAllocation: TsCheckBox;
    CBXallowPriceList: TsCheckBox;
    CBXallowPrices: TsCheckBox;
    CBXallowStock: TsCheckBox;
    sLabel4: TsLabel;
    EDTcompanyAggregator: TsEdit;
    CBXallowSalesOut: TsCheckBox;
    CBXallowOtherPay: TsCheckBox;
    CBXallowRewards: TsCheckBox;
    procedure FormShow(Sender: TObject);
    procedure ACTaddressAndDocExecute(Sender: TObject);
    procedure BTclearCriteriaClick(Sender: TObject);
    procedure ACTchgPortalInfoExecute(Sender: TObject);
    procedure CDSlistAfterScroll(DataSet: TDataSet);
    procedure BTNokClick(Sender: TObject);
    procedure BTNcancelClick(Sender: TObject);
    procedure cxDBVmasterFocusedRecordChanged(
      Sender: TcxCustomGridTableView; APrevFocusedRecord,
      AFocusedRecord: TcxCustomGridRecord;
      ANewItemRecordFocusingChanged: Boolean);
    procedure PGCportalInfoChange(Sender: TObject);
  private
```

#### **Lcustomer.dfm**

```
inherited FORMLcustomer: TFORMLcustomer
  Left = 433
  Top = 152
  Caption = 'Customers List'
  ClientHeight = 686
  ClientWidth = 1078
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  PixelsPerInch = 96
  TextHeight = 13
  inherited SPLlist: TsSplitter
    Top = 225
    Width = 1078
  end
  inherited PNLsearchArea: TsPanel
    Width = 1078
    Height = 181
    inherited PNLsearchButtons: TsPanel
      Left = 984
      Height = 179
      TabOrder = 0
      inherited BTsearch: TsBitBtn
        ParentFont = True
      end
      inherited BTclearCriteria: TsBitBtn
        ParentFont = True
      end
    end
    inherited SRBcriteria: TsScrollBox
      Width = 983
      Height = 179
      TabOrder = 1
      inherited PNLcriteria: TsPanel
        Width = 979
        Height = 175
        inline FRAMEfindCriteriaListCustomer1: TFRAMEfindCriteriaListCustomer
          Left = 1
          Top = 1
          Width = 977
          Height = 173
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          inherited sPageControl1: TsPageControl
            Width = 977
            Height = 173
            inherited SHcriteria: TsTabSheet
              inherited sLabel2: TsLabel
                FocusControl = FRAMEfindCriteriaListCustomer1.FRAMEfindCustMarket.FE
              end
              inherited Label10: TsLabel
                FocusControl = FRAMEfindCriteriaListCustomer1.FRAMEfindGroup.FE
              end
              inherited LBL1: TsLabel
                FocusControl = FRAMEfindCriteriaListCustomer1.FRAMEfindSeller.FE
              end
              inherited CBObusChannel: TcxImageComboBox
                Width = 122
              end
              inherited CBOstat: TcxImageComboBox
                Width = 99
              end
            end
          end
        end
      end
    end
  end
  inherited PNLactions: TsPanel
    Width = 1078
    inherited CLBlistActions: TsCoolBar
      Width = 1076
      Bands = <
        item
          Break = False
          Control = PNLstandardActions
          ImageIndex = -1
          MinHeight = 40
          Width = 1072
        end>
      inherited PNLstandardActions: TsPanel
        Width = 1059
        inherited BTNseparator: TsSpeedButton
          Width = 9
        end
        inherited PNLformActions: TsPanel
          Left = 1012
        end
        object PNL1: TsPanel
          Left = 502
          Top = 1
          Width = 83
          Height = 38
          Align = alLeft
```
<!-- tabs:end -->

